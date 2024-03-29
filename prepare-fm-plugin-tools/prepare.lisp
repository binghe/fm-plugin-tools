;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PREPARE-FM-PLUGIN-TOOLS; Base: 10 -*-

;;; Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2021-2022, Chun Tian (binghe).  All rights reserved.

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

(in-package :prepare-fm-plugin-tools)

(defun handle-typedef (line)
  "Accepts a string which is supposed to be a simple C typedef.
Stores a corresponding entry in *TYPEDEFS*."
  (when (scan "typedef(?!.*[A-Z_])" line)
    (register-groups-bind (existing-type defined-type)
        ("typedef\\s+(.*)(?<!\\s)\\s+(\\w+);" line)      
      (pushnew (cons (make-fli-type defined-type)
                     (make-fli-type existing-type))
               *typedefs*
               :key #'car))))

(defun read-enum-value (string)
  "Reads the optional value part of a C enum and returns a
corresponding Lisp value - either a number or a LOGIOR
expression."
  ;; convert hex marker for Lisp reader
  (setq string (regex-replace-all "0x" string "#x"))
  (if (scan "\\|" string)
    ;; contains a pipe symbol, so make LOGIOR of previously defined
    ;; constants
    (let (result)
      (do-matches-as-strings (value "[#\\w]+" string)
	;; Piped constants and numbers appeared in SDK version 17.
	;; -- Chun Tian (binghe), 1 sep 2018.
	(cond ((eq (elt value 0) #\#)
	       (push (read-from-string value) result))
	      (t
	       (push (mangle-name value t) result))))
      (cons 'logior (nreverse result)))
    ;; just read value as a number
    (read-from-string string)))

(defun write-function-definition (lisp-name c-name result-type args)
  "Accepts values which suffice to create a foreign function
defintion and writes it to the output stream."
  ;; we use DEFINE-FMXCPT-FUNCTION as defined in FM-PLUGIN-TOOLS
  (pprint `(fm-plugin-tools::define-fmxcpt-function (,lisp-name ,c-name)
               ,(loop for (type name nil) in (butlast args)
                      collect `(,name ,(if (and #-:win32 nil
                                                (string= c-name "FM_Text_AssignUnicode")
                                                (string-equal name "s"))
                                         ;; special case for this one function (only on Windows) -
                                         ;; pass Lisp string directly as an argument
                                         '(:reference-pass (:ef-wc-string :external-format :unicode))
                                         type)))
             :result-type ,result-type)))

(defun handle-function (line)
  "Accepts one line of C code and checks if it's a function prototype.
If it is one, we write a corresponding function definition to the
output stream."
  ;; all `interesting' prototypes use the FMX_API macro - we just have
  ;; to throw away the lines where this macro is defined
  (when (and (scan "FMX_API.*&_x" line)
             (not (scan "#define" line)))
    (setq line (simplify line))
    ;; the part between the parens are the arguments - that's
    ;; simple... :)
    (register-groups-bind (head args)
        ("(.*)\\((.*)\\);" line)
      (destructuring-bind (result-type lisp-name c-name)
          (type-and-name head)
        (write-function-definition lisp-name c-name result-type
                                   ;; args are separated by commas
                                   (loop for arg in (split "," args)
                                         collect (type-and-name arg t)))))))

(defun handle-enum (body)
  "Handles the part between `enum {' and `}'.  Loops through all
lines, writes one DEFCONSTANT per item and the corresponding
EXPORT statement."
  (let ((counter 0))
    (do-register-groups (name value)
        ("(?m)^\\s*(\\w+)\\s*(?:=\\s*([^,/]*\\z|.*?)\\s*)?(?:,|(?:/.*)?$)" body)
      ;; use value if provided in enum, COUNTER otherwise
      (setq value (if value (read-enum-value value) counter))
      (let ((lisp-name (mangle-name name t)))
        (pprint `(eval-when (:compile-toplevel :load-toplevel :execute)
                   (defconstant ,lisp-name ,value
                     "This constant was generated automatically.
See FileMaker header files for details.")))
        (print `(export ',lisp-name :fm-plugin-tools)))
      ;; increment counter or continue with successor of value
      (setq counter (1+ (if (numberp value) value counter))))))

(defun handle-struct (struct-name body pack)
  "Handles the part between `struct {' and `}' - writes a
corresponding FLI:DEFINE-C-STRUCT definition.  If PACK is non-NIL (a number),
byte-packing will be used."
  (let (slots)
    (do-register-groups (prefix type name)
        ;; for some reason FMX_PACK isn't used in 8.5 anymore
        ("(?m)^\\s*(fmx::)?(\\w+)\\s+(\\w+)(?:\\s*FMX_PACK)?\\s*;(?:\\s*//.*)?\\s*?$" body)
      (declare (ignore prefix)) ; e.g. "fmx::unusedid" in 19
      (push (list (cond ((scan "FMX_" type)
                         ;; default types which start with `FMX_' to :VOID
                         (find-type (make-fli-type (regex-replace "FMX_" type ""))
                                    '(:pointer :void)))
                        (t
                         (find-type (make-fli-type type))))
                  (mangle-name name)
                  pack)
            slots))
    (pprint `(fli:define-c-struct ,(mangle-name struct-name)
               ,@(loop for first = t then nil
                       for (slot-type slot-name pack) in (nreverse slots)
                       when (and pack (not first))
                       collect `(:byte-packing ,pack)
                       collect `(,slot-name ,(if (and (string= struct-name "FMX_ExternCallStruct")
                                                      (string-equal slot-name "which-call"))
                                               ;; special for this one slot
                                               '(:unsigned :char)
                                               slot-type)))))))

;; NOTE: something has changed after we changed to prepare on FMXExtern.hhh.
;; In the original FMXExtern.h, for GCC there's the following definition
;;
;; #define FMX_PACK_ON
;;
;; Note that there's no whitespaces after the word "FMX_PACK_ON", and it turns
;; out that "gcc -E" will not replace it at all, leaving "#pragma FMX_PACK_ON"
;; UNCHANGED in the *.hhh files. On the other hand, for Windows it's defined as
;;
;; #define FMX_PACK_ON                 pack (push, 1)
;;
;; Now, very funny, "cl /E" will replace all "#pragma FMX_PACK_ON" with
;; "#pragma pack (push, 1)", causing the pack flag wrongly recognized in the
;; following code with the original pattern:
;;
;; "(?sm)(#pragma\\s+FMX_PACK_ON\\s+)?^\\s*struct (\\w+)$(\\s*){(.*?)\\3}
;;
;; The new pattern below solves the plugin loading issue on FMP Windows (64-bit).
;;
;; Note also that, on macOS, even "#pragma FMX_PACK_ON" is there and FMX_PACK_ON
;; is undefined, it should be understood as still packing, otherwise the plugin
;; does't load at all.  -- Chun Tian, 18/7/2022

(defparameter *if-regex1*
  (create-scanner "#ifdef\\s+(.*)"))

(defparameter *if-regex2*
  (create-scanner "#(el)?if\\s+(!)?([\\w\\s\\|\\(\\)<>!=_&]+)(?<!\\s)\\s*$"))

(defun parse-header-files ()
  "Loops through all C header files in *HEADER-FILE-NAMES*,
checks for enums, structs or function prototypes and writes the
corresponding C code to *STANDARD-OUTPUT*."
  (dolist (name *header-file-names*)
    (let ((header-file (make-pathname :name name :type "h"
                                       :defaults *fmx-extern-location*))
          (file-string (make-array '(0)
                                   :element-type 'simple-char
                                   :fill-pointer 0
                                   :adjustable t))
          (*line-number* 0))
      (format t "~%;; #include <~A.h>" name)
      (format *error-output* "Processing ~A.h...~%" name)
      (with-open-file (in header-file)
        (with-output-to-string (out file-string)
          (loop with contexts = '(:error)     ; the polarity of the current #if context
                with pos-contexts = '(:error) ; the current #if context when polarity is T
                with neg-contexts = '(:error) ; the current #if context when polarity is NIL
                for line = (read-line in nil nil)
                while line do
            (incf *line-number*)
            (cond ((scan "#ifndef.*" line) ; usually only first line of a header
                   (push :enable contexts))
                  ;; ifdef ...
                  ((scan *if-regex1* line)
                   (register-groups-bind (context) (*if-regex1* line)
                     (setq context (regex-replace-all "\\s+" context " "))
                     (if (member context *negative-macros* :test 'equal)
                         (push :disable contexts)
                         (push :enable contexts))))
                  ;; (el)if [!]...
                  ((scan *if-regex2* line)
                   (register-groups-bind (elif-p neg-p context) (*if-regex2* line)
                     (when elif-p ; #elif = #endif + #if (not #else + #if !!!)
                       (let ((context (pop contexts)))
                         (ecase context
                           ((:enable :disable)
                            t)
                           (:positive
                            (pop pos-contexts))
                           (:negative
                            (pop neg-contexts)))))
                     (setq context (regex-replace-all "\\s+" context " "))
                     (if (or (member context *positive-macros* :test 'equal)
                             (member context *negative-macros* :test 'equal))
                         (cond (neg-p
                                (push context neg-contexts)
                                (push :negative contexts))
                               (t
                                (push context pos-contexts)
                                (push :positive contexts)))
                       ;; an irrelevant condition, we choose the first branch
                       (push :enable contexts))))
                  ((scan "#(el)?if\\s+.*" line) ; the fallback case of #if
                   (register-groups-bind (elif-p)
                       ("#(el)?if\\s+.*" line)
                     (when elif-p ; #elif = #else + #if
                       (let ((context (pop contexts)))
                         (ecase context
                           ((:enable :disable)
                            t)
                           (:positive
                            (pop pos-contexts))
                           (:negative
                            (pop neg-contexts)))))
                     ;; :enable by default for dont-care #if
                     (push :enable contexts)))
                  ;; turn over the context if we met #else
                  ((scan "#else" line)
                   (let ((context (pop contexts)))
                     (ecase context
                       (:enable  (push :disable contexts))
                       (:disable (push :enable  contexts))
                       (:positive
                        (push (pop pos-contexts) neg-contexts)
                        (push :negative contexts))
                       (:negative
                        (push (pop neg-contexts) pos-contexts)
                        (push :positive contexts)))))
                  ;; pop the current context
                  ((scan "#endif" line)
                   (let ((context (pop contexts)))
                     (ecase context
                       ((:enable :disable)
                        t)
                       (:positive
                        (pop pos-contexts))
                       (:negative
                        (pop neg-contexts)))))
                  (t
                   (cond ((not (null (intersection *negative-macros* pos-contexts :test 'equal)))
                          ;; (format *error-output* "ignored this line~%")
                          nil) ; ignore this line
                         ((not (null (intersection *positive-macros* neg-contexts :test 'equal)))
                          ;; (format *error-output* "ignored this line~%")
                          nil) ; ignore this line
                         ((member :disable contexts)
                          ;; (format *error-output* "ignored this line~%")
                          nil)
                         (t
                          ;; single-line processing
                          (handle-typedef line)
                          (handle-function line)
                          ;; multi-line processing (preparation)
                          (format out "~A~%" line)))))
                ;; still inside the loop
                #+ignore
                (format *error-output* "L~D contexts: ~A, pos-contexts: ~A, neg-contexts: ~A~%"
                        *line-number* contexts pos-contexts neg-contexts)
                )))
      (do-register-groups (enum-body)
          ("(?s)enum(?:\\s+\\w+)?\\s*\\{\\s*(.*?)\\s*,?\\s*\\}" file-string)
        (handle-enum enum-body))
      (do-register-groups (pack name whitespace struct-body)
          ;; "pack (push, 1)" is a macro which translates to (:BYTE-PACKING 1)
          ("(?sm)(#pragma.*)?^\\s*struct (\\w+)$(\\s*){(.*?)\\3}"
           file-string)
        (declare (ignore whitespace))
        (handle-struct name struct-body
          ;; NEW: now we decide if packing is needed in a backward compatible way
          (cond ((null pack)
                 nil) ; no #pragma at all, no packing
                ((scan "FMX_PACK_ON" pack)
                 #+win32 nil ; with #pragma but FMX_PACK_ON is undefined
                 #-win32 1)  ; always pack on macOS (or it doesn't load)
                ((scan "pack \\(push, 1\\)" pack)
                 1)   ; with #pragma and FMX_PACK_ON is "pack (push, 1)"
                (t
                 (error "new, unknown #pragma occurs now!")))))
      (terpri))))

(defun prepare ()
  "Creates the missing file `fli.lisp' for FM-PLUGIN-TOOLS from
the C header files of FileMaker Pro Advanced."
  ;; find out where to look for headers
  (unless *fmx-extern-location*
    (set-fmx-extern-location))
  ;; redirect *STANDARD-OUTPUT* to `fli.lisp'
  (with-open-file (*standard-output* *fli-file*
                                     :direction :output
                                     :if-exists :supersede)
    ;; use correct package for output and refrain from writing
    ;; everything in uppercase
    (with-standard-io-syntax 
      (let ((*package* (find-package :fm-plugin-tools))
            (*print-case* :downcase))
        (format t ";;; This file was generated automatically from FileMaker Pro's SDK headers.")
        (terpri)        
        (print '(in-package :fm-plugin-tools))
        (terpri)
        ;; let this function do all the work
        (parse-header-files))))
  :done)
