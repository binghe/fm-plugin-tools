;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-

;;; Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2022, Chun Tian (binghe).  All rights reserved.

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

(in-package :fm-plugin-tools)

;; About "compatibleOnFlags"
;;
;; DEPRECATED in FileMaker Pro 12. The server database process no longer loads
;; plug-ins. Stored calculations containing plug-in functions will not evaluate
;; the same way they do on a client with the plug-in if a schema change forces
;; the server to recalculate the stored calculation's value.
;;
;;     kMayEvaluateOnServer    = 0x00000001,

;; DEPRECATED in FileMaker Pro 16. All the following flags will be ignored for
;; the commented purpose in version 16 and later.
;; Note the change of kDisplayInAllDialogs further below.
;;
;;     kDisplayCalcFields      = 0x00000100,   // Calculated fields
;;     kDisplayAutoEnter       = 0x00000200,   // Auto enter calculation
;;     kDisplayValidation      = 0x00000400,   // Validation by calculation
;;     kDisplayCustomFunctions = 0x00000800,   // Custom function definition
;;     kDisplayPrivileges      = 0x00001000,   // Row level access calculations
;;     kDisplayInFuture1       = 0x00002000,   // As yet undefined calculation dialog
;;     kDisplayInFuture2       = 0x00004000,   // As yet undefined calculation dialog
;;     kDisplayGeneric         = 0x00008000,   // Dialog used by scripting and replace
;;
;; Changed in FileMaker Pro 16v2. If any of these bits are set, the function
;; will be displayed in any picking UI control. Future functions should use this
;; constant if they want to be displayed. The function can always be typed in manually.
;;     kDisplayInAllDialogs    = 0x0000FF00,

;; New to FileMaker Pro 16 (API VERSION 57) and later. Bits used in the renamed/new
;; compatibleOnFlags parameter. If all the kFutureCompatible bits are zero then it is
;; treated as if all the kFutureCompatible bits were set.
;;     kMacCompatible          = 0x00000002,
;;     kWinCompatible          = 0x00000004,
;;     kServerCompatible       = 0x00000008,
;;     kIOSCompatible          = 0x00000010,
;;     kCustomWebCompatible    = 0x00000020,
;;     kWebDirectCompatible    = 0x00000040,
;;     kAllDeviceCompatible    = kMacCompatible | kWinCompatible | kServerCompatible
;;                             | kIOSCompatible | kCustomWebCompatible | kWebDirectCompatible,
;;     kFutureCompatible       = kAllDeviceCompatible | 0x00FF0000,
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-type-flags*
    (logior +k-display-in-all-dialogs+ ; Changed in FileMaker Pro 16v2
            +k-may-evaluate-on-server+ ; DEPRECATED in FileMaker Pro 12
            +k-all-device-compatible+
            +k-future-compatible+)))

;; Dynamic Registration of Plug-in Functions
;;
;; RegisterExternalFunction enables the plug-in to register a function with
;; the application, so that function appears in the calculation dialog in
;; the application.
;;
;; "pluginId" should be the unique four-digit identifier for your plug-in that
;;    you use for the "options" string.
;; "functionId" is the unique id that you can use to represent which function
;;    was called, it will be passed back to the registered function as the
;;    first parameter (see the parameter of the same name in "ExtPluginType").
;; "functionName" is the name of the function as it should appear in the
;;    calculation formula.
;; "functionPrototype" is the suggested syntax that will appear in the list of
;;    functions in the calculation dialog.
;; "minArgs" is the number of required parameters for the function.  0 is the
;;    smallest valid value.
;; "maxArgs" is the maximum number of parameters that they user should be able
;;     to specify in the calculation dialog  and still have correct syntax
;;     usage for the function.  Use -1 to allow a variable number of parameters
;;     up to the number supported by calculation formulas in the application.
;; "compatibleOnFlags" see bit flags above.
;; "funcPtr" is the pointer to the function that must match the signature defined
;;     by ExtPluginType.
;;
;; UnRegisterExternalFunction removes the function entry from the list of external
;; functions in any calculation dialog.
;;
(defun fm-expr-env-register-external-function*
       (function-id function-name function-prototype func-ptr
        &key (min-args 0) (max-args -1) (type-flags #.*default-type-flags*))
  "This is just a convenience wrapper for FM-EXPR-ENV-REGISTER-EXTERNAL-FUNCTION."
  (with-quadchar (plugin-id *plugin-id*)
    (fm-expr-env-register-external-function plugin-id
       function-id function-name function-prototype
       min-args (or max-args -1)
       type-flags func-ptr)))

(defun fm-expr-env-un-register-external-function* (function-id)
  "This is just a convenience wrapper for FM-EXPR-ENV-UN-REGISTER-EXTERNAL-FUNCTION."
  (with-quadchar (plugin-id *plugin-id*)
    (fm-expr-env-un-register-external-function plugin-id function-id)))

;; New to FileMaker Pro 15 (API VERSION 56) and later
;; Same as RegisterExternalFunction except with additional parameter for the
;; define calculation dialog
;;
;; "functionDescription" format is "type ahead word list|description text"
;;
;; If this version is not used to register the function, the description is
;; generated by setting the "type ahead word list" to the function name with all
;; underscore characters converted to spaces and leaving the "description text" blank
;; (i.e. BE_FileType_All becomes "BE FileType All|")

(defun fm-expr-env-register-external-function-ex*
       (function-id function-name function-prototype
        function-description ; new
        func-ptr
        &key (min-args 0) (max-args -1) (type-flags #.*default-type-flags*))
  "This is just a convenience wrapper for FM-EXPR-ENV-REGISTER-EXTERNAL-FUNCTION."
  (with-quadchar (plugin-id *plugin-id*)
    (fm-expr-env-register-external-function-ex plugin-id
       function-id function-name function-prototype
       function-description ; new
       min-args (or max-args -1)
       type-flags func-ptr)))

;; New to FileMaker Pro 16 (API VERSION 57) and later
;; Dynamic Registration of Script Steps
;;
;; RegisterScriptStep enables the plug-in to register a script step with the application,
;; so that script step appears in the scripting workspace in the application.
;;
;; "pluginId" should be the unique four-digit identifier for your plug-in that
;;    you use for the "options" string.
;; "scriptStepId" is the unique id that you can use to represent which script step
;;    was called, it will be passed back to the registered function as the first
;;    parameter (see the parameter of the same name in "ExtPluginType").
;; "scriptStepName" is the name of the script step as it should appear in the scripting
;;    workspace.
;; "scriptStepDefinition" is the XML definition of what parameters should be displayed
;;    for the script step.
;; "scriptStepDescription" is the description of the script step that is presented to
;;    the user when the step is selected.  This uses the same syntax as the external
;;    function registration method.
;; "compatibleOnFlags" see bit flags above.
;; "funcPtr" is the pointer to the function that must match the signature defined by
;;    ExtPluginType.
;;
;; UnRegisterScriptStep removes the script step entry from the list of script steps
;; in the scripting workspace.
;;
(defun fm-expr-env-register-script-step*
       (script-step-id
        script-step-name
        script-step-definition
        script-step-description
        func-ptr &key (type-flags #.*default-type-flags*))
  "This is just a convenience wrapper for FM-EXPR-ENV-REGISTER-SCRIPT-STEP"
  (with-quadchar (plugin-id *plugin-id*)
    (fm-expr-env-register-script-step
       plugin-id script-step-id
       script-step-name
       script-step-definition
       script-step-description
       type-flags func-ptr)))

(defun fm-expr-env-un-register-script-step* (script-step-id)
  "This is just a convenience wrapper for FM-EXPR-ENV-UN-REGISTER-SCRIPT-STEP."
  (with-quadchar (plugin-id *plugin-id*)
    (fm-expr-env-un-register-script-step plugin-id script-step-id)))

(defun handle-get-string-message (which-string lang-id result-size result-ptr)
  "Handles `kFMXT_GetString' messages from FileMaker.
WHICH-STRING is the ID for the information FileMaker wants to
have, RESULT-PTR is where the answer string is supposed to be
stored, and RESULT-SIZE is the maximal size of the result."
  (declare (ignore lang-id))
  (let* ((string (case which-string
                   (#.+k-fmxt-name-str+       *plugin-name*)
                   (#.+k-fmxt-app-config-str+ *plugin-help-text*)
                   (#.+k-fmxt-help-urlstr+    *plugin-help-url*)
                   (#.+k-fmxt-options-str+    (create-options-string))))
         (pointer (make-pointer :address result-ptr
                                :type #+win32 :wchar-t
                                      #-win32 :unsigned-short)))
    (if string
        (progn
          #+:win32
          (convert-to-foreign-string string
                                     :limit (1- result-size)
                                     :external-format :unicode
                                     :into pointer)
          #-:win32
          (loop with ptr = pointer
                for index from 0 below (1- result-size)
                for char across string
                do (setf (dereference ptr :index index) (char-code char))
                finally (setf (dereference ptr :index index) 0))) ; was: #\Null
      ;; else... mainly for the future
      (setf (dereference pointer :index 0) 0))))

(defun register-plugin-functions (version)
  "Loops through *PLUGIN-FUNCTIONS* and registers with FileMaker
all functions which were defined with DEFINE-PLUGIN-FUNCTION."
  (let ((prefix (string-append *plugin-id* "_")))
    (dolist (tuple *plugin-functions*)
      (destructuring-bind (function-id prototype
                           documentation ; new
                           c-name min-args max-args flags)
          tuple
        (with-text (name% (string-append prefix (function-name prototype)))
          (with-text (prototype% (string-append prefix (string-trim " " prototype)))
            (let* ((type-flags (or flags #.*default-type-flags*))
                   (err-code
                    (cond ((and documentation (<= +k150extn-version+ version))
                           (with-text (documentation% documentation)
                             (fm-expr-env-register-external-function-ex*
                                function-id
                                name%
                                prototype%
                                documentation%
                                (make-pointer :symbol-name c-name)
                                :min-args min-args
                                :max-args max-args
                                :type-flags type-flags)))
                          (t
                           (fm-expr-env-register-external-function*
                                function-id
                                name%
                                prototype%
                                (make-pointer :symbol-name c-name)
                                :min-args min-args
                                :max-args max-args
                                :type-flags type-flags)))))
              (unless (zerop err-code)
                (fm-log "Got error code ~A while registering function ~S.~%"
                        err-code (function-name prototype))))))))))

(defun register-plugin-script-steps ()
  "Loops through *PLUGIN-SCRIPT-STEPS* and registers with FileMaker
all script steps which were defined with DEFINE-PLUGIN-SCRIPT-STEP."
  (dolist (tuple *plugin-script-steps*)
    (destructuring-bind (script-step-id script-step-name
                         definition documentation c-name flags)
        tuple
      (with-text (name% script-step-name)
        (with-text (definition% definition)
          (with-text (description% documentation)
            (let* ((type-flags (or flags #.*default-type-flags*))
                   (err-code
                    (fm-expr-env-register-script-step*
                       script-step-id name% definition% description%
                       (make-pointer :symbol-name c-name)
                       :type-flags type-flags)))
              (unless (zerop err-code)
                (fm-log "Got error code ~A while registering script step ~S.~%"
                        err-code script-step-name)))))))))

(defun handle-init-message (version)
  "Handles `kFMXT_Init' messages from FileMaker.  Version is the
database version as sent by FileMaker.  The function is supposed
to return +K-CURRENT-EXTN-VERSION+ if everything is OK."
  (flet ((do-not-enable (condition)
           "Local handler which logs errors during this phase and
refrains from enabling the plug-in."
           (ignore-errors
             (fm-log "Plug-in ~A not enabled: ~A.~%" *plugin-name* condition)
             (when *log-backtraces-p*
               (fm-log "Backtrace:~% ~A~%" (get-backtrace))))
           (return-from handle-init-message +k-do-not-enable+)))
    (handler-bind ((error #'do-not-enable))
      ;; here we connect to the "FMWrapper" shared library (DLL or
      ;; framework) which lives inside the FileMaker folder
      (register-module :fm-wrapper
                       ;; immediate, so we get an error here if
                       ;; something goes wrong
                       :connection-style :immediate
                       :real-name
                       #+:win32 "FMWrapper"
                       ;; relative syntax introduced for dlopen in OS X 10.4
                       #+:macosx "@executable_path/../Frameworks/FMWrapper.framework/Versions/A/FMWrapper")
      ;; check version
      (unless (<= +k70extn-version+ version +k-max-extn-version+)
        (fm-log "Plug-in ~A not enabled because of wrong version ~A.~%"
                *plugin-name* version)
        (return-from handle-init-message +k-bad-extn-version+))
      ;; prepare for Windows registy entries
      (set-product-name)
      (setf (sys:product-registry-path :fm-plugin-tools)
            (list "Software" *company-name* *product-name*))
      ;; call user-provided init function first if there is one
      (when *init-function*
        (funcall *init-function*))
      ;; register plug-in functions
      (register-plugin-functions version)
      ;; register plug-in script steps
      (when (<= +k150extn-version+ version)
        (register-plugin-script-steps))
      ;; set *filemaker-version*, this is the version of hosting FileMaker Pro
      (setq *fm-version* version)
      ;; This is essentially the version of SDK headers
      +k-current-extn-version+)))

(defun unregister-plugin-functions ()
  "Loops through *PLUGIN-FUNCTIONS* and unregisters with
FileMaker all functions which were defined with
DEFINE-PLUGIN-FUNCTION."
  (dolist (tuple *plugin-functions*)
    (destructuring-bind (function-id prototype &rest rest)
        tuple
      (declare (ignore rest))
      (let ((err-code (fm-expr-env-un-register-external-function* function-id)))
        (unless (zerop err-code)
          (fm-log "Got error code ~A while unregistering function ~S.~%"
                  err-code (function-name prototype)))))))

(defun unregister-plugin-script-steps ()
  "Loops through *PLUGIN-SCRIPT-STEPS* and unregisters with
FileMaker all script steps which were defined with
DEFINE-PLUGIN-SCRIPT-STEP."
  (dolist (tuple *plugin-script-steps*)
    (destructuring-bind (script-step-id script-step-name &rest rest)
        tuple
      (declare (ignore rest))
      (let ((err-code (fm-expr-env-un-register-script-step* script-step-id)))
        (unless (zerop err-code)
          (fm-log "Got error code ~A while unregistering script step ~S.~%"
                  err-code script-step-name))))))

(defun handle-shutdown-message (version)
  "Handles `kFMXT_Shutdown' messages from FileMaker."
  ;; unregister plug-in functions
  (unregister-plugin-functions)
  ;; unregister plug-in script steps
  (when (<= +k150extn-version+ version)
    (unregister-plugin-script-steps))
  ;; call user-provided shutdown function if there is one
  (when *shutdown-function*
    (funcall *shutdown-function*))
  ;; unregister the :FM-WRAPPER module  
  (disconnect-module :fm-wrapper)
  ;; free global environment if necessary
  (when *global-environment*
    (ignore-errors
      (fm-expr-env-delete *global-environment*)))
  ;; force quit the DLL
  (unless (lw:dll-quit)
    (fm-log "LW:DLL-QUIT didn't succeed.  Diagnostic output follows below.~%")
    (fm-log "Trying \(LW:DLL-QUIT :FORCE T) now.~%")
    (multiple-value-bind (success output)
        (lw:dll-quit :force t :output nil)
      (fm-log "~A" output)
      (fm-log "~&\(LW:DLL-QUIT :FORCE T) ~:[failed~;succeeded~].~%" success))))

(defun handle-app-preferences-message ()
  "Handles `kFMXT_DoAppPreferences' messages from FileMaker."
  ;; we only call the user's function if there is one
  #-:macosx
  (when *preferences-function*
    (funcall *preferences-function*)))

(defun handle-idle-message-internal (idle-level &optional session-id)
  "Handles `kFMXT_Idle' messages from FileMaker.  Calls
HANDLE-IDLE-MESSAGE."
  (declare (ignore session-id))
  ;; collect all generations \(but not too often, see *GC-INTERVAL*) if
  ;; the user is idle.
  (when (and (= idle-level +k-fmxt-user-idle+)
             *gc-interval*
             (> (- (get-universal-time) *last-gc*)
                *gc-interval*))
    (gc-generation #+:lispworks-32bit 3
                   #+:lispworks-64bit :blocking-gen-num)
    (setq *last-gc* (get-universal-time)))
  ;; maybe some user code will be called here
  (handle-idle-message idle-level))

(defgeneric handle-idle-message (idle-level)
  (:documentation "Called with corresponding level when FileMaker
is idle.  Can be specialized by plug-in authors."))

(defmethod handle-idle-message (idle-level)
  "The default method which does nothing.")

;; a pointer to the C struct define by PREPARE-FM-PLUGIN-TOOLS
(define-c-typedef fmx-extern-call-ptr
  (:pointer fmx-extern-call-struct))

;; The meanings of parm1..parm3 in terms of the various messages:
;;  Msg =                   Parm1                  Parm2                    Parm3
;;  --------------------------------------------------------------------------------
;;  kFMXT_Init              FMX_Application value  App vers unicode c str*  [unused]
;;  kFMXT_Idle              FMX_IdleLevel value    Session ID               [unused]
;; [kFMXT_External]         [unused]               Funct str index          Parameter text**
;;  kFMXT_Shutdown          [unused]               [unused]                 [unused]
;;  kFMXT_DoAppPreferences  [unused]               [unused]                 [unused]
;;  kFMXT_GetString         FMX_Strings value      Windows lang ID          Maximum size of string to return
;;  kFMXT_SessionShutdown   [unused]               Session ID               [unused]
;;  kFMXT_FileShutdown      [unused]               Session ID               File ID

(define-foreign-callable ("FMExternCallProc" :result-type :void
                                             :calling-convention :cdecl)
    ((parameter-block fmx-extern-call-ptr))
  "The way FileMaker calls into our plug-in.  See FileMaker
documentation for details."
  ;; remember value in global variable, so callbacks have access to it -
  ;; see definition of START-SCRIPT
  (setq *parameter-block* parameter-block)
  ;; dispatch to handlers defined above
  (case (which-call)
    (#.+k-fmxt-get-string+         ; REQUIRED to be handled
     (handle-get-string-message (parm1) (parm2) (parm3) (result)))
    (#.+k-fmxt-idle+               ; Enabled by kFMXT_OptionsStr character 9
     (handle-idle-message-internal (parm1) (parm2)))
    (#.+k-fmxt-init+               ; Enabled by kFMXT_OptionsStr character 8
     (setf (result) (handle-init-message (extn-version))))
    (#.+k-fmxt-shutdown+           ; Enabled by kFMXT_OptionsStr character 8
     (handle-shutdown-message (extn-version)))
    (#.+k-fmxt-do-app-preferences+ ; Enabled by kFMXT_OptionsStr character 6
     (handle-app-preferences-message))
    ;; Below are new to FileMaker Pro 15 (API VERSION 56) and later
    (#.+k-fmxt-session-shutdown+   ; Enabled by kFMXT_OptionsStr character 10
     nil)   ; not implemented
    (#.+k-fmxt-file-shutdown+      ; Enabled by kFMXT_OptionsStr character 10
     nil)   ; not implemented
    ))
