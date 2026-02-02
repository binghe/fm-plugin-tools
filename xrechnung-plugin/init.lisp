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

;; Keep jzon symbols from being removed during delivery
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'com.inuoe.jzon:parse *symbols-to-keep*)
  (pushnew 'com.inuoe.jzon:stringify *symbols-to-keep*)
  (pushnew 'com.inuoe.jzon::parser *symbols-to-keep*))

;; Configure FM-PLUGIN-TOOLS for this particular plug-in
(setq *plugin-id* "XRec"
      *plugin-name* "XRechnungPlugin"
      #+:macosx #+:macosx
      *plugin-bundle-identifier* "de.jensteich.XRechnungPlugin"
      *plugin-help-text* "A plug-in for generating X-Rechnung compliant invoices and PDF/A documents."
      ;; see ASDF system definition
      *plugin-version* cl-user:*xrechnung-plugin-version*
      *company-name* "Jens Teich"
      *copyright-message* "Copyright (c) 2026, Jens Teich.  All rights reserved."
      *log-backtraces-p* t
      ;; the function defined in configuration.lisp
      *preferences-function* #'handle-configuration
      ;; enable idle messages for garbage collection
      *enable-idle-messages* t
      ;; defined in utils.lisp - reads configuration values when initialized
      *init-function* #'read-config-values)
