;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: XRECHNUNG-PLUGIN; Base: 10 -*-

;;; Copyright (c) 2026, Jens Teich.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :xrechnung-plugin)

;;; ===========================================================================
;;; X-Rechnung XML Generation (CII Format)
;;; ===========================================================================

(defparameter *cii-namespaces*
  '(("rsm" . "urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100")
    ("qdt" . "urn:un:unece:uncefact:data:standard:QualifiedDataType:100")
    ("ram" . "urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100")
    ("udt" . "urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100"))
  "Standard namespaces for CII (Cross Industry Invoice) format.")

(defun write-xml-element (stream name content &key attributes namespace)
  "Write an XML element with optional attributes and namespace."
  (let ((full-name (if namespace
                      (format nil "~A:~A" namespace name)
                      name)))
    (format stream "<~A" full-name)
    (when attributes
      (loop for (key . value) in attributes
            do (format stream " ~A=\"~A\"" key (sanitize-xml-string (princ-to-string value)))))
    (if content
        (progn
          (format stream ">")
          (if (stringp content)
              (write-string (sanitize-xml-string content) stream)
              (funcall content stream))
          (format stream "</~A>~%" full-name))
        (format stream "/>~%"))))

(defmacro with-xml-element ((stream name &key attributes namespace) &body body)
  "Macro for writing XML elements with body content."
  (let ((full-name (if namespace
                      `(format nil "~A:~A" ,namespace ,name)
                      name)))
    `(progn
       (format ,stream "<~A" ,full-name)
       ,@(when attributes
           `((loop for (key . value) in ,attributes
                   do (format ,stream " ~A=\"~A\"" key
                            (sanitize-xml-string (princ-to-string value))))))
       (format ,stream ">~%")
       ,@body
       (format ,stream "</~A>~%" ,full-name))))

(defun write-cii-header (stream)
  "Write the CII XML header and root element."
  (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
  (format stream "<rsm:CrossIndustryInvoice")
  (loop for (prefix . uri) in *cii-namespaces*
        do (format stream " xmlns:~A=\"~A\"" prefix uri))
  (format stream " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"")
  (format stream " xsi:schemaLocation=\"urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100 CrossIndustryInvoice_100pD16B.xsd\"")
  (format stream ">~%"))

(defun write-exchange-context (stream)
  "Write the ExchangedDocumentContext section."
  (with-xml-element (stream "ExchangedDocumentContext" :namespace "rsm")
    (with-xml-element (stream "GuidelineSpecifiedDocumentContextParameter" :namespace "ram")
      (write-xml-element stream "ID" "urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0"
                        :namespace "ram"))))

(defun write-document-header (stream invoice-data)
  "Write the ExchangedDocument section with invoice metadata."
  (let ((invoice-number (gethash "invoiceNumber" invoice-data))
        (invoice-date (gethash "invoiceDate" invoice-data))
        (invoice-type (or (gethash "invoiceTypeCode" invoice-data) "380"))
        (notes (gethash "notes" invoice-data)))

    (with-xml-element (stream "ExchangedDocument" :namespace "rsm")
      ;; Invoice number
      (write-xml-element stream "ID" invoice-number :namespace "ram")

      ;; Invoice type code (380 = Commercial Invoice)
      (write-xml-element stream "TypeCode" invoice-type :namespace "ram")

      ;; Invoice date
      (with-xml-element (stream "IssueDateTime" :namespace "ram")
        (write-xml-element stream "DateTimeString" invoice-date
                          :namespace "udt"
                          :attributes '(("format" . "102"))))

      ;; Notes (if any)
      (when notes
        (loop for note in (if (listp notes) notes (list notes))
              do (with-xml-element (stream "IncludedNote" :namespace "ram")
                   (write-xml-element stream "Content" note :namespace "ram")))))))

(defun write-party (stream party-data tag-name)
  "Write a party (seller/buyer) section."
  (with-xml-element (stream tag-name :namespace "ram")
    ;; Party name
    (write-xml-element stream "Name" (gethash "name" party-data) :namespace "ram")

    ;; Postal address
    (let ((address (gethash "address" party-data)))
      (when address
        (with-xml-element (stream "PostalTradeAddress" :namespace "ram")
          (write-xml-element stream "PostcodeCode" (gethash "postalCode" address) :namespace "ram")
          (write-xml-element stream "LineOne" (gethash "street" address) :namespace "ram")
          (write-xml-element stream "CityName" (gethash "city" address) :namespace "ram")
          (write-xml-element stream "CountryID" (or (gethash "countryCode" address) "DE") :namespace "ram"))))

    ;; Tax registration (for seller)
    (let ((vat-id (gethash "vatId" party-data))
          (tax-number (gethash "taxNumber" party-data)))
      (when vat-id
        (with-xml-element (stream "SpecifiedTaxRegistration" :namespace "ram")
          (write-xml-element stream "ID" vat-id
                            :namespace "ram"
                            :attributes '(("schemeID" . "VA")))))
      (when tax-number
        (with-xml-element (stream "SpecifiedTaxRegistration" :namespace "ram")
          (write-xml-element stream "ID" tax-number
                            :namespace "ram"
                            :attributes '(("schemeID" . "FC"))))))

    ;; Contact information
    (let ((contact (gethash "contact" party-data)))
      (when contact
        (with-xml-element (stream "DefinedTradeContact" :namespace "ram")
          (when (gethash "name" contact)
            (write-xml-element stream "PersonName" (gethash "name" contact) :namespace "ram"))
          (when (gethash "phone" contact)
            (with-xml-element (stream "TelephoneUniversalCommunication" :namespace "ram")
              (write-xml-element stream "CompleteNumber" (gethash "phone" contact) :namespace "ram")))
          (when (gethash "email" contact)
            (with-xml-element (stream "EmailURIUniversalCommunication" :namespace "ram")
              (write-xml-element stream "URIID" (gethash "email" contact) :namespace "ram"))))))))

(defun write-line-item (stream item index)
  "Write a single invoice line item."
  (with-xml-element (stream "IncludedSupplyChainTradeLineItem" :namespace "ram")
    ;; Line ID
    (with-xml-element (stream "AssociatedDocumentLineDocument" :namespace "ram")
      (write-xml-element stream "LineID" (princ-to-string index) :namespace "ram"))

    ;; Product/Service description
    (with-xml-element (stream "SpecifiedTradeProduct" :namespace "ram")
      (write-xml-element stream "Name" (gethash "description" item) :namespace "ram")
      (when (gethash "articleNumber" item)
        (write-xml-element stream "SellerAssignedID" (gethash "articleNumber" item) :namespace "ram")))

    ;; Line trade agreement (prices)
    (with-xml-element (stream "SpecifiedLineTradeAgreement" :namespace "ram")
      (with-xml-element (stream "NetPriceProductTradePrice" :namespace "ram")
        (write-xml-element stream "ChargeAmount"
                          (format-decimal (gethash "unitPrice" item))
                          :namespace "ram")))

    ;; Line trade delivery (quantity)
    (with-xml-element (stream "SpecifiedLineTradeDelivery" :namespace "ram")
      (with-xml-element (stream "BilledQuantity" :namespace "ram")
        (write-string (format-decimal (gethash "quantity" item) 0) stream))
      (format stream "</ram:BilledQuantity>~%"))

    ;; Line trade settlement (tax and totals)
    (let ((quantity (gethash "quantity" item))
          (unit-price (gethash "unitPrice" item))
          (vat-rate (gethash "vatRate" item)))
      (with-xml-element (stream "SpecifiedLineTradeSettlement" :namespace "ram")
        ;; VAT
        (with-xml-element (stream "ApplicableTradeTax" :namespace "ram")
          (write-xml-element stream "TypeCode" "VAT" :namespace "ram")
          (write-xml-element stream "CategoryCode" "S" :namespace "ram")
          (write-xml-element stream "RateApplicablePercent"
                            (format-decimal vat-rate)
                            :namespace "ram"))

        ;; Line total
        (with-xml-element (stream "SpecifiedTradeSettlementLineMonetarySummation" :namespace "ram")
          (write-xml-element stream "LineTotalAmount"
                            (format-decimal (* quantity unit-price))
                            :namespace "ram"))))))

(defun calculate-totals (line-items currency)
  "Calculate invoice totals from line items."
  (let ((net-total 0)
        (tax-amounts (make-hash-table :test 'equal)))

    ;; Calculate net total and tax amounts per rate
    (loop for item across line-items
          for quantity = (gethash "quantity" item)
          for unit-price = (gethash "unitPrice" item)
          for vat-rate = (gethash "vatRate" item)
          for line-net = (* quantity unit-price)
          do (incf net-total line-net)
             (let ((tax-amount (* line-net (/ vat-rate 100))))
               (incf (gethash vat-rate tax-amounts 0) tax-amount)))

    ;; Calculate total tax
    (let ((total-tax 0))
      (maphash (lambda (rate amount)
                 (declare (ignore rate))
                 (incf total-tax amount))
               tax-amounts)

      (values net-total
              total-tax
              (+ net-total total-tax)
              tax-amounts))))

(defun write-monetary-summation (stream line-items currency)
  "Write the monetary summation section with all totals."
  (multiple-value-bind (net-total tax-total gross-total tax-by-rate)
      (calculate-totals line-items currency)

    (with-xml-element (stream "SpecifiedTradeSettlementHeaderMonetarySummation" :namespace "ram")
      ;; Net total
      (write-xml-element stream "LineTotalAmount"
                        (format-decimal net-total)
                        :namespace "ram")

      ;; Tax basis
      (write-xml-element stream "TaxBasisTotalAmount"
                        (format-decimal net-total)
                        :namespace "ram")

      ;; Total tax
      (write-xml-element stream "TaxTotalAmount"
                        (format-decimal tax-total)
                        :namespace "ram"
                        :attributes `(("currencyID" . ,currency)))

      ;; Gross total
      (write-xml-element stream "GrandTotalAmount"
                        (format-decimal gross-total)
                        :namespace "ram")

      ;; Due payable amount
      (write-xml-element stream "DuePayableAmount"
                        (format-decimal gross-total)
                        :namespace "ram"))

    ;; Return tax breakdown for use in tax section
    tax-by-rate))

(defun write-trade-transaction (stream invoice-data)
  "Write the SupplyChainTradeTransaction section."
  (let ((line-items (gethash "lineItems" invoice-data))
        (seller (gethash "seller" invoice-data))
        (buyer (gethash "buyer" invoice-data))
        (currency (or (gethash "currency" invoice-data) "EUR"))
        (payment-terms (gethash "paymentTerms" invoice-data)))

    (with-xml-element (stream "SupplyChainTradeTransaction" :namespace "rsm")

      ;; Line items
      (loop for item across line-items
            for index from 1
            do (write-line-item stream item index))

      ;; Trade agreement (parties)
      (with-xml-element (stream "ApplicableHeaderTradeAgreement" :namespace "ram")
        (write-party stream seller "SellerTradeParty")
        (write-party stream buyer "BuyerTradeParty"))

      ;; Trade delivery (delivery information)
      (with-xml-element (stream "ApplicableHeaderTradeDelivery" :namespace "ram")
        (let ((delivery-date (gethash "deliveryDate" invoice-data)))
          (when delivery-date
            (with-xml-element (stream "ActualDeliverySupplyChainEvent" :namespace "ram")
              (with-xml-element (stream "OccurrenceDateTime" :namespace "ram")
                (write-xml-element stream "DateTimeString" delivery-date
                                  :namespace "udt"
                                  :attributes '(("format" . "102"))))))))

      ;; Trade settlement (payment and totals)
      (with-xml-element (stream "ApplicableHeaderTradeSettlement" :namespace "ram")
        ;; Currency
        (write-xml-element stream "InvoiceCurrencyCode" currency :namespace "ram")

        ;; Payment terms
        (when payment-terms
          (with-xml-element (stream "SpecifiedTradePaymentTerms" :namespace "ram")
            (write-xml-element stream "Description" payment-terms :namespace "ram")
            (let ((due-date (gethash "dueDate" invoice-data)))
              (when due-date
                (with-xml-element (stream "DueDateDateTime" :namespace "ram")
                  (write-xml-element stream "DateTimeString" due-date
                                    :namespace "udt"
                                    :attributes '(("format" . "102"))))))))

        ;; Tax totals (per rate)
        (let ((tax-by-rate (write-monetary-summation stream line-items currency)))
          (maphash (lambda (rate amount)
                    (with-xml-element (stream "ApplicableTradeTax" :namespace "ram")
                      (write-xml-element stream "CalculatedAmount"
                                        (format-decimal amount)
                                        :namespace "ram")
                      (write-xml-element stream "TypeCode" "VAT" :namespace "ram")
                      (write-xml-element stream "BasisAmount"
                                        (format-decimal (/ amount (/ rate 100)))
                                        :namespace "ram")
                      (write-xml-element stream "CategoryCode" "S" :namespace "ram")
                      (write-xml-element stream "RateApplicablePercent"
                                        (format-decimal rate)
                                        :namespace "ram")))
                  tax-by-rate))))))

(defun generate-xrechnung-xml-to-stream (stream invoice-data)
  "Generate complete X-Rechnung XML to a stream."
  (write-cii-header stream)
  (write-exchange-context stream)
  (write-document-header stream invoice-data)
  (write-trade-transaction stream invoice-data)
  (format stream "</rsm:CrossIndustryInvoice>~%"))

(defun generate-xrechnung-xml (invoice-data output-path)
  "Generate X-Rechnung XML file from invoice data.
Returns output path on success, error message on failure."
  (handler-case
      (progn
        ;; Validate required fields
        (unless (gethash "invoiceNumber" invoice-data)
          (return-from generate-xrechnung-xml "Error: Missing required field: invoiceNumber"))
        (unless (gethash "invoiceDate" invoice-data)
          (return-from generate-xrechnung-xml "Error: Missing required field: invoiceDate"))
        (unless (gethash "seller" invoice-data)
          (return-from generate-xrechnung-xml "Error: Missing required field: seller"))
        (unless (gethash "buyer" invoice-data)
          (return-from generate-xrechnung-xml "Error: Missing required field: buyer"))
        (unless (gethash "lineItems" invoice-data)
          (return-from generate-xrechnung-xml "Error: Missing required field: lineItems"))

        ;; Generate XML
        (with-open-file (stream output-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
          (generate-xrechnung-xml-to-stream stream invoice-data))

        (namestring output-path))
    (error (e)
      (format nil "Error generating X-Rechnung XML: ~A" e))))
