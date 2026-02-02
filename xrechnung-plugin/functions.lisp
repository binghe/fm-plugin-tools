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
;;; Version and Information Functions
;;; ===========================================================================

(define-plugin-function "Version"
    ()
  "Returns the version of XRechnungPlugin as a string."
  (version-string))

(define-plugin-function "GetXRechnungVersion"
    ()
  "Returns the X-Rechnung standard version supported by this plugin."
  *xrechnung-version*)

(define-plugin-function "GetPDFALevel"
    ()
  "Returns the PDF/A conformance level (e.g., '3b')."
  *pdf-a-level*)

(define-plugin-function "CheckTools"
    ()
  "Checks if required external tools (Ghostscript, QPDF) are available.
Returns: JSON string with tool availability status."
  (let ((gs-available (ghostscript-available-p))
        (qpdf-available (qpdf-available-p)))
    (format nil "{\"ghostscript\": ~A, \"qpdf\": ~A, \"ghostscript_path\": \"~A\", \"qpdf_path\": \"~A\"}"
            (if gs-available "true" "false")
            (if qpdf-available "true" "false")
            (or gs-available "not found")
            (or qpdf-available "not found"))))

;;; ===========================================================================
;;; PDF/A Conversion Functions
;;; ===========================================================================

(define-plugin-function "ConvertToPDFAFromFile ( pdfPath ; outputPath {; metadata} )"
    ((pdf-path :string) (output-path :string) &optional (metadata :string))
  "Converts a PDF file to PDF/A-3b format.
pdfPath: Path to input PDF file
outputPath: Full path where the PDF/A file should be saved
metadata: Optional JSON string with PDF metadata (title, author, subject, keywords)
Returns: Path to created PDF/A file on success, error message on failure."
  (handler-case
      (progn
        ;; Check if Ghostscript is available
        (unless (ghostscript-available-p)
          (return-from convert-to-pdfa-from-file
            "Error: Ghostscript not found. Please install Ghostscript to enable PDF/A conversion."))

        ;; Verify input file exists
        (unless (probe-file pdf-path)
          (return-from convert-to-pdfa-from-file
            (format nil "Error: Input PDF file not found: ~A" pdf-path)))

        ;; Build Ghostscript command
        (let* ((error-log (merge-pathnames
                          (format nil "gs-error-~A.log" (get-universal-time))
                          (get-temp-directory)))
               (gs-cmd (get-ghostscript-command))
               (command (format nil "~A -dPDFA=3 -dBATCH -dNOPAUSE -dNOSAFER ~
                                    -sColorConversionStrategy=RGB ~
                                    -sDEVICE=pdfwrite ~
                                    -dPDFACompatibilityPolicy=1 ~
                                    -sOutputFile=~A ~A"
                               (quote-shell-arg gs-cmd)
                               (quote-shell-arg (namestring output-path))
                               (quote-shell-arg pdf-path))))

          ;; Execute Ghostscript
          (multiple-value-bind (exit-code output)
              (run-external-command command :error-output-file error-log)

            ;; Check result
            (if (zerop exit-code)
                (progn
                  (ignore-errors (delete-file error-log))
                  (namestring output-path))
                (let ((error-msg (if (probe-file error-log)
                                   (with-open-file (stream error-log)
                                     (with-output-to-string (str)
                                       (loop for line = (read-line stream nil)
                                             while line
                                             do (write-line line str))))
                                   output)))
                  (ignore-errors (delete-file error-log))
                  (format nil "Error: Ghostscript conversion failed (exit code ~A): ~A"
                         exit-code error-msg))))))
    (error (e)
      (format nil "Error converting to PDF/A: ~A" e))))

(define-plugin-function "ConvertToPDFA ( pdfData ; outputPath {; metadata} )"
    ((pdf-data :binary) (output-path :string) &optional (metadata :string))
  "Converts a FileMaker PDF to PDF/A-3b format.
pdfData: Binary PDF data from FileMaker (e.g., from a container field)
outputPath: Full path where the PDF/A file should be saved
metadata: Optional JSON string with PDF metadata (title, author, subject, keywords)
Returns: Path to created PDF/A file on success, error message on failure."
  (handler-case
      (progn
        ;; Check if Ghostscript is available
        (unless (ghostscript-available-p)
          (return-from convert-to-pdfa
            "Error: Ghostscript not found. Please install Ghostscript to enable PDF/A conversion."))

        ;; Create temporary input file
        (ensure-temp-directory)
        (let* ((temp-input (merge-pathnames
                           (format nil "input-~A.pdf" (get-universal-time))
                           (get-temp-directory)))
               (error-log (merge-pathnames
                          (format nil "gs-error-~A.log" (get-universal-time))
                          (get-temp-directory))))

          ;; Write binary data to temporary file
          (write-binary-to-file pdf-data temp-input)

          ;; Build Ghostscript command
          (let* ((gs-cmd (get-ghostscript-command))
                 (command (format nil "~A -dPDFA=3 -dBATCH -dNOPAUSE -dNOSAFER ~
                                      -sColorConversionStrategy=RGB ~
                                      -sDEVICE=pdfwrite ~
                                      -dPDFACompatibilityPolicy=1 ~
                                      -sOutputFile=~A ~A"
                                 (quote-shell-arg gs-cmd)
                                 (quote-shell-arg (namestring output-path))
                                 (quote-shell-arg (namestring temp-input)))))

            ;; Execute Ghostscript
            (multiple-value-bind (exit-code output)
                (run-external-command command :error-output-file error-log)

              ;; Clean up temporary input file
              (ignore-errors (delete-file temp-input))

              ;; Check result
              (if (zerop exit-code)
                  (progn
                    (ignore-errors (delete-file error-log))
                    (namestring output-path))
                  (let ((error-msg (if (probe-file error-log)
                                     (with-open-file (stream error-log)
                                       (with-output-to-string (str)
                                         (loop for line = (read-line stream nil)
                                               while line
                                               do (write-line line str))))
                                     output)))
                    (ignore-errors (delete-file error-log))
                    (format nil "Error: Ghostscript conversion failed (exit code ~A): ~A"
                           exit-code error-msg)))))))
    (error (e)
      (format nil "Error converting to PDF/A: ~A" e))))

(define-plugin-function "ValidatePDFA ( pdfPath )"
    ((pdf-path :string))
  "Validates if a PDF file is PDF/A-3b compliant.
Returns: '1' if valid, '0' if not valid, or error message."
  (handler-case
      (progn
        ;; Check if file exists
        (unless (probe-file pdf-path)
          (return-from validate-pdfa
            (format nil "Error: PDF file not found: ~A" pdf-path)))

        ;; Check if Ghostscript is available
        (unless (ghostscript-available-p)
          (return-from validate-pdfa
            "Error: Ghostscript not found. Please install Ghostscript to enable PDF/A validation."))

        ;; Use Ghostscript to validate PDF/A compliance
        ;; This is a basic check - for production use, consider veraPDF
        (let* ((temp-output (merge-pathnames
                            (format nil "validate-~A.pdf" (get-universal-time))
                            (get-temp-directory)))
               (error-log (merge-pathnames
                          (format nil "validate-error-~A.log" (get-universal-time))
                          (get-temp-directory)))
               (gs-cmd (get-ghostscript-command))
               (command (format nil "~A -dPDFA=3 -dBATCH -dNOPAUSE -dNOSAFER ~
                                    -sDEVICE=pdfwrite ~
                                    -dPDFACompatibilityPolicy=2 ~
                                    -sOutputFile=~A ~A"
                               (quote-shell-arg gs-cmd)
                               (quote-shell-arg (namestring temp-output))
                               (quote-shell-arg pdf-path))))

          ;; Execute Ghostscript validation
          (multiple-value-bind (exit-code output)
              (run-external-command command :error-output-file error-log)

            ;; Clean up temporary files
            (ignore-errors (delete-file temp-output))

            ;; Check error log for PDF/A issues
            (let ((errors (when (probe-file error-log)
                           (with-open-file (stream error-log)
                             (with-output-to-string (str)
                               (loop for line = (read-line stream nil)
                                     while line
                                     when (or (search "error" line :test #'char-equal)
                                            (search "warning" line :test #'char-equal))
                                     do (write-line line str)))))))

              (ignore-errors (delete-file error-log))

              (if (and (zerop exit-code)
                      (or (null errors) (zerop (length errors))))
                  "1"  ;; Valid PDF/A
                  (if (zerop exit-code)
                      (format nil "0 - Warnings: ~A" errors)  ;; Valid but with warnings
                      (format nil "0 - Validation failed: ~A" errors)))))))  ;; Invalid
    (error (e)
      (format nil "Error validating PDF/A: ~A" e))))

;;; ===========================================================================
;;; X-Rechnung XML Generation Functions
;;; ===========================================================================

(define-plugin-function "GenerateXRechnungXML ( invoiceData ; outputPath )"
    ((invoice-data :string) (output-path :string))
  "Generates X-Rechnung compliant XML from invoice data.
invoiceData: JSON string containing all invoice information
outputPath: Full path where the XML file should be saved
Returns: Path to created XML file on success, error message on failure."
  (handler-case
      (let ((data (parse-json-string invoice-data)))
        ;; TODO: Implement X-Rechnung XML generation
        ;; This must create a CII (Cross Industry Invoice) XML file
        ;; conforming to EN 16931 and X-Rechnung specification

        ;; Generate XML structure
        (with-open-file (stream output-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
          ;; Write XML header
          (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
          ;; TODO: Write actual X-Rechnung XML structure
          (format stream "<!-- X-Rechnung XML generation not fully implemented -->~%"))

        output-path)
    (error (e)
      (format nil "Error generating X-Rechnung XML: ~A" e))))

(define-plugin-function "ValidateXRechnungXML ( xmlPath )"
    ((xml-path :string))
  "Validates X-Rechnung XML against the specification.
Returns: '1' if valid, '0' if not valid, or error message with details."
  (handler-case
      (let ((cache-key xml-path))
        ;; Check cache first
        (when *cache-validation-p*
          (let ((cached (gethash cache-key *validation-cache*)))
            (when cached
              (return-from validate-xrechnung-xml cached))))

        ;; TODO: Implement XML validation
        ;; This must validate against:
        ;; - X-Rechnung XSD schema
        ;; - Schematron rules
        ;; - Business rules (BR-DE-XX)

        (let ((result "Validation not yet implemented"))
          (when *cache-validation-p*
            (setf (gethash cache-key *validation-cache*) result))
          result))
    (error (e)
      (format nil "Error validating XML: ~A" e))))

;;; ===========================================================================
;;; ZUGFeRD/Factur-X Hybrid Functions
;;; ===========================================================================

(define-plugin-function "CreateZUGFeRD ( pdfPath ; xmlPath ; outputPath )"
    ((pdf-path :string) (xml-path :string) (output-path :string))
  "Creates a ZUGFeRD/Factur-X hybrid document by embedding XML into PDF/A.
pdfPath: Path to PDF/A-3b file
xmlPath: Path to X-Rechnung XML file
outputPath: Path where the hybrid document should be saved
Returns: Path to created hybrid file on success, error message on failure."
  (handler-case
      (progn
        ;; Check if QPDF is available
        (unless (qpdf-available-p)
          (return-from create-zugferd
            "Error: QPDF not found. Please install QPDF to enable XML embedding."))

        ;; Verify input files exist
        (unless (probe-file pdf-path)
          (return-from create-zugferd
            (format nil "Error: PDF file not found: ~A" pdf-path)))

        (unless (probe-file xml-path)
          (return-from create-zugferd
            (format nil "Error: XML file not found: ~A" xml-path)))

        ;; Build QPDF command to embed XML as attachment
        (let* ((error-log (merge-pathnames
                          (format nil "qpdf-error-~A.log" (get-universal-time))
                          (get-temp-directory)))
               (command (format nil "qpdf ~A ~A --add-attachment ~A ~
                                    --filename=factur-x.xml ~
                                    --mimetype=text/xml ~
                                    --description=\"Factur-X Invoice\" ~
                                    --replace"
                               (quote-shell-arg pdf-path)
                               (quote-shell-arg output-path)
                               (quote-shell-arg xml-path))))

          ;; Execute QPDF
          (multiple-value-bind (exit-code output)
              (run-external-command command :error-output-file error-log)

            ;; Check result
            (if (zerop exit-code)
                (progn
                  (ignore-errors (delete-file error-log))
                  (namestring output-path))
                (let ((error-msg (if (probe-file error-log)
                                   (with-open-file (stream error-log)
                                     (with-output-to-string (str)
                                       (loop for line = (read-line stream nil)
                                             while line
                                             do (write-line line str))))
                                   output)))
                  (ignore-errors (delete-file error-log))
                  (format nil "Error: QPDF embedding failed (exit code ~A): ~A"
                         exit-code error-msg))))))
    (error (e)
      (format nil "Error creating ZUGFeRD: ~A" e))))

(define-plugin-function "ExtractXMLFromZUGFeRD ( zugferdPath ; outputPath )"
    ((zugferd-path :string) (output-path :string))
  "Extracts the embedded XML from a ZUGFeRD/Factur-X document.
Returns: Path to extracted XML file on success, error message on failure."
  (handler-case
      (progn
        ;; Check if QPDF is available
        (unless (qpdf-available-p)
          (return-from extract-xml-from-zugferd
            "Error: QPDF not found. Please install QPDF to enable XML extraction."))

        ;; Verify input file exists
        (unless (probe-file zugferd-path)
          (return-from extract-xml-from-zugferd
            (format nil "Error: ZUGFeRD file not found: ~A" zugferd-path)))

        ;; Create temporary directory for extraction
        (let* ((temp-dir (ensure-temp-directory))
               (error-log (merge-pathnames
                          (format nil "qpdf-extract-error-~A.log" (get-universal-time))
                          temp-dir))
               ;; First, list attachments to find the XML
               (list-command (format nil "qpdf ~A --show-attachment=factur-x.xml"
                                   (quote-shell-arg zugferd-path))))

          ;; Try to extract factur-x.xml attachment
          (multiple-value-bind (exit-code output)
              (run-external-command list-command :error-output-file error-log)

            (if (zerop exit-code)
                ;; Attachment exists, extract it
                (let ((extract-command (format nil "qpdf ~A --show-attachment=factur-x.xml > ~A"
                                             (quote-shell-arg zugferd-path)
                                             (quote-shell-arg output-path))))
                  (multiple-value-bind (extract-exit extract-output)
                      (run-external-command extract-command :error-output-file error-log)

                    (ignore-errors (delete-file error-log))

                    (if (zerop extract-exit)
                        (namestring output-path)
                        (format nil "Error: Failed to extract XML (exit code ~A): ~A"
                               extract-exit extract-output))))
                ;; Try alternative filename zugferd-invoice.xml
                (let ((alt-command (format nil "qpdf ~A --show-attachment=zugferd-invoice.xml > ~A"
                                         (quote-shell-arg zugferd-path)
                                         (quote-shell-arg output-path))))
                  (multiple-value-bind (alt-exit alt-output)
                      (run-external-command alt-command :error-output-file error-log)

                    (ignore-errors (delete-file error-log))

                    (if (zerop alt-exit)
                        (namestring output-path)
                        (format nil "Error: No XML attachment found in PDF. ~
                                   Tried factur-x.xml and zugferd-invoice.xml"))))))))
    (error (e)
      (format nil "Error extracting XML: ~A" e))))

;;; ===========================================================================
;;; Invoice Data Validation Functions
;;; ===========================================================================

(define-plugin-function "ValidateInvoiceData ( invoiceData )"
    ((invoice-data :string))
  "Validates invoice data against X-Rechnung business rules.
invoiceData: JSON string containing invoice information
Returns: '1' if valid, or JSON string with validation errors."
  (handler-case
      (let ((data (parse-json-string invoice-data)))
        ;; TODO: Implement business rules validation
        ;; Check required fields:
        ;; - Invoice number
        ;; - Invoice date
        ;; - Seller information (name, address, tax ID)
        ;; - Buyer information (name, address)
        ;; - Line items with proper structure
        ;; - Tax calculations
        ;; - Total amounts

        "Validation not yet implemented")
    (error (e)
      (format nil "Error validating invoice data: ~A" e))))

(define-plugin-function "ValidateVATID ( vatId )"
    ((vat-id :string))
  "Validates VAT ID format.
Returns: '1' if format is valid, '0' otherwise."
  (if (validate-vat-id vat-id)
      "1"
      "0"))

;;; ===========================================================================
;;; Utility Functions
;;; ===========================================================================

(define-plugin-function "FormatDateXRechnung ( fmDate )"
    ((fm-date :date))
  "Formats a FileMaker date to X-Rechnung date format (YYYY-MM-DD).
Returns: Formatted date string."
  (format-date-xrechnung fm-date))

(define-plugin-function "ClearValidationCache"
    ()
  "Clears the validation cache.
Returns: Number of entries removed."
  (let ((count (cache-size)))
    (clear-validation-cache)
    (format nil "~D" count)))

;;; ===========================================================================
;;; Helper Functions (not exported as plugin functions)
;;; ===========================================================================

(defun parse-json-string (json-string)
  "Parse a JSON string into a Lisp data structure.
This is a placeholder - you'll need a proper JSON library."
  ;; TODO: Implement with a proper JSON library (e.g., com.gigamonkeys.json or jonathan)
  (declare (ignore json-string))
  nil)
