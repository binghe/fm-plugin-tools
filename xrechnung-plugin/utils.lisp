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

(defun version-string ()
  "Returns the version of XRechnungPlugin as a string."
  (format nil "~{~A~^.~}" *plugin-version*))

(defun read-config-values ()
  "Read configuration values from registry/preferences.
Called during plug-in initialization."
  ;; Read cache setting
  (multiple-value-bind (cache-value foundp)
      (plugin-preference "Configuration" "CacheValidation")
    (when foundp
      (setq *cache-validation-p* cache-value)))

  ;; Read temporary directory setting if stored
  (multiple-value-bind (temp-dir foundp)
      (plugin-preference "Configuration" "TempDirectory")
    (when foundp
      (setq *temp-directory* temp-dir))))

(defun store-config-values ()
  "Store configuration values to registry/preferences."
  ;; Store cache setting
  (setf (plugin-preference "Configuration" "CacheValidation")
        *cache-validation-p*)

  ;; Store temporary directory if set
  (when *temp-directory*
    (setf (plugin-preference "Configuration" "TempDirectory")
          *temp-directory*)))

(defun get-xrechnung-temp-directory ()
  "Returns the temporary directory path, creating it if necessary."
  (or *temp-directory*
      (setq *temp-directory*
            #+:win32
            (merge-pathnames "XRechnungPlugin/" (sys:get-folder-path :appdata))
            #-:win32
            (merge-pathnames ".xrechnung-plugin/" (user-homedir-pathname)))))

(defun ensure-temp-directory ()
  "Ensure the temporary directory exists."
  (let ((dir (get-xrechnung-temp-directory)))
    (ensure-directories-exist dir)
    dir))

(defun format-date-xrechnung (universal-time)
  "Format a Universal Time to X-Rechnung date format (YYYY-MM-DD)."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore sec min hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun format-decimal (number &optional (precision 2))
  "Format a number as decimal string with specified precision."
  (format nil "~,VF" precision number))

(defun validate-vat-id (vat-id)
  "Basic validation of VAT ID format.
Returns T if format appears valid, NIL otherwise."
  ;; Basic check: should start with 2-letter country code
  (and (stringp vat-id)
       (>= (length vat-id) 4)
       (alpha-char-p (char vat-id 0))
       (alpha-char-p (char vat-id 1))))

(defun sanitize-xml-string (string)
  "Sanitize a string for use in XML content.
Replaces XML special characters."
  (when string
    (with-output-to-string (out)
      (loop for char across (string string)
            do (case char
                 (#\& (write-string "&amp;" out))
                 (#\< (write-string "&lt;" out))
                 (#\> (write-string "&gt;" out))
                 (#\" (write-string "&quot;" out))
                 (#\' (write-string "&apos;" out))
                 (t (write-char char out)))))))

(defun clear-validation-cache ()
  "Clear the validation cache."
  (clrhash *validation-cache*))

(defun cache-size ()
  "Return the number of entries in the validation cache."
  (hash-table-count *validation-cache*))

;;; ===========================================================================
;;; External Tool Integration
;;; ===========================================================================

(defun find-executable (name)
  "Try to find an executable in common locations.
Returns the full path if found, NIL otherwise."
  #+:win32
  (let ((common-paths '("C:/Program Files/gs/" "C:/Program Files (x86)/gs/"
                        "C:/gs/" "C:/qpdf/bin/"))
        (extensions '("" ".exe" ".bat" ".cmd")))
    (dolist (dir common-paths)
      (dolist (ext extensions)
        (let ((full-path (concatenate 'string dir name ext)))
          (when (probe-file full-path)
            (return-from find-executable full-path))))))
  #-:win32
  ;; On macOS/Linux, check common paths
  (let ((common-paths '("/usr/local/bin/" "/usr/bin/" "/opt/homebrew/bin/"
                        "/opt/local/bin/" "/bin/")))
    (dolist (dir common-paths)
      (let ((full-path (concatenate 'string dir name)))
        (when (probe-file full-path)
          (return-from find-executable full-path))))))

(defun ghostscript-available-p ()
  "Check if Ghostscript is available on the system."
  (or (find-executable "gs")
      #+:win32 (find-executable "gswin64c")
      #+:win32 (find-executable "gswin32c")))

(defun qpdf-available-p ()
  "Check if QPDF is available on the system."
  (find-executable "qpdf"))

(defun get-ghostscript-command ()
  "Get the Ghostscript command name for this platform."
  (or (find-executable "gs")
      #+:win32 (find-executable "gswin64c")
      #+:win32 (find-executable "gswin32c")
      "gs"))

(defun run-external-command (command &key error-output-file)
  "Run an external command and return (values exit-code output).
Uses sys:call-system to avoid stream issues."
  (declare (ignore error-output-file))
  (handler-case
      (let ((exit-code (sys:call-system command :current-directory nil)))
        (values exit-code ""))
    (error (e)
      (values -1 (format nil "Error running command: ~A" e)))))

(defun quote-shell-arg (arg)
  "Quote a shell argument for safe execution."
  #+:win32
  (format nil "\"~A\"" arg)
  #-:win32
  (format nil "'~A'" (substitute-if-not
                      #\'
                      (lambda (c) (char/= c #\'))
                      arg)))

(defun write-binary-to-file (binary-data filepath)
  "Write binary data to a file."
  (with-open-file (stream filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
    (write-sequence binary-data stream))
  filepath)
