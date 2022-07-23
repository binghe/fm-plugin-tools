;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-

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

(define-foreign-funcallable fmx-set-to-current-env
    ((environment (:pointer :void)))
  :documentation "Updates the `FMX_ExprEnv' struct pointed to by
ENVIRONMENT with the \"current\" environment.  Probably.  This feature
is not really documented by FileMaker."
  :result-type :short)

(defun update-global-environment ()
  "Sets the global special variable *GLOBAL-ENVIRONMENT* \(used for
example as a fallback by EVALUATE) to a new `FMX_ExprEnv' struct which
is then updated by a call to FMX-SET-TO-CURRENT-ENV.  The old struct
pointed to by *GLOBAL-ENVIRONMENT* \(if any) is freed.  Must be called
from within a function that is called by FileMaker.

This function is protected with a lock, so you don't need to care
about concurrency."
  ;; update is within a lock so we don't confuse readers
  (mp:with-lock (*global-environment-lock*)
    (let (old-global-environment done)
      (unwind-protect
          (progn
            (setq old-global-environment *global-environment*
                  *global-environment* (fm-expr-env-constructor1))
            ;; update
            (fmx-set-to-current-env (c-current-env) *global-environment*)
            (setq done t))
        (when old-global-environment
          (ignore-errors
            ;; free old struct
            (fm-expr-env-delete old-global-environment)))
        (unless done
          ;; try to free if something went wrong
          (ignore-errors
            (fm-expr-env-delete *global-environment*))
          (setq *global-environment* nil))))))

(defgeneric evaluate (expression &optional result)
  (:documentation "Evaluates \(as with FileMaker's `Evaluate'
function) the expression EXPRESSION.  The result is stored in the
DATA-OBJECT RESULT which is created if none is provided.  The
function returns RESULT."))

(defmethod evaluate ((expression text-object) &optional (result (make-data-object)))
  (let ((err-code (fm-expr-env-evaluate (get-environment)
                                        (pointer expression)
                                        (pointer result))))
    (unless (zerop err-code)
      (error "Got error code ~A while evaluating ~S."
             err-code (as-string expression))))
  result)

(defmethod evaluate ((expression string) &optional (result (make-data-object)))
  (evaluate (make-text-object expression) result))

;; New to FileMaker Pro 18 (API VERSION 60) and later.
;; Values for each of the calculation engine's Get() functions
(defparameter *get-function-value-map*
  '((#.+k-get-application-version+ "ApplicationVersion" :application-version)
    (#.+k-get-current-date+        "CurrentDate"        :current-date)
    (#.+k-get-last-error+          "LastError"          :last-error)
    (#.+k-get-active-field-name+   "ActiveFieldName"    :active-field-name)
    (#.+k-get-file-name+           "FileName"           :file-name)
    (#.+k-get-file-size+           "FileSize"           :file-size)
    (#.+k-get-found-count+         "FoundCount"         :found-count)
    (#.+k-get-host-name+           "HostName"           :host-name)
    (#.+k-get-layout-count+        "LayoutCount"        :layout-count)
    (#.+k-get-layout-name+         "LayoutName"         :layout-name)
    (#.+k-get-layout-number+       "LayoutNumber"       :layout-number)
    (#.+k-get-system-language+     "SystemLanguage"     :system-language)
    (#.+k-get-window-mode+         "WindowMode"         :window-mode)
    (#.+k-get-multi-user-state+    "MultiUserState"     :multi-user-state)
    (#.+k-get-page-number+         "PageNumber"         :page-number)
    (#.+k-get-system-platform+     "SystemPlatform"     :system-platform)
    (#.+k-get-active-portal-row-number+
                                   "ActivePortalRowNumber" :active-portal-rwo-number)
    (#.+k-get-printer-name+        "PrinterName"        :printer-name)
    (#.+k-get-total-record-count+  "TotalRecordCount"   :total-record-count)
    (#.+k-get-record-number+       "RecordNumber"       :record-number)
    (#.+k-get-active-repetition-number+ "ActiveRepetitionNumber" :active-repetition-number)
    (#.+k-get-request-count+       "RequestCount"       :request-count)
    (#.+k-get-screen-depth+        "ScreenDepth"        :screen-depth)
    (#.+k-get-screen-height+       "ScreenHeight"       :screen-height)
    (#.+k-get-screen-width+        "ScreenWidth"        :screen-width)
    (#.+k-get-script-name+         "ScriptName"         :script-name)
    (#.+k-get-sort-state+          "SortState"          :sort-state)
    (#.+k-get-system-version+      "SystemVersion"      :system-version)
    (#.+k-get-current-time+        "CurrentTime"        :current-time)
    (#.+k-get-user-count+          "UserCount"          :user-count)
    (#.+k-get-user-name+           "UserName"           :user-name)
    (#.+k-get-account-name+        "AccountName"        :account-name)
    (#.+k-get-last-message-choice+ "LastMessageChoice"  :last-message-choice)
    (#.+k-get-current-privilege-set-name+
                                   "CurrentPrivilegeSetName" :current-privilege-set-name)
    (#.+k-get-active-modifier-keys+ "ActiveModifierKeys" :active-modifier-keys)
    (#.+k-get-network-protocol+    "NetworkProtocol"    :network-protocol)
    (#.+k-get-record-id+           "RecordID"           :record-id)
    (#.+k-get-record-modification-count+ "RecordModificationCount" :record-modification-count)
    (#.+k-get-active-field-contents+ "ActiveFieldContents" :active-field-contents)
    (#.+k-get-file-path+           "FilePath"           :file-path)
    (#.+k-get-last-external-error-detail+
                                   "LastExternalErrorDetail" :last-external-error-detail)
    (#.+k-get-layout-access+       "LayoutAccess"       :layout-access)
    (#.+k-get-record-access+       "RecordAccess"       :record-accees)
    (#.+k-get-high-contrast-state+ "HighContrastState"  :high-contrast-state)
    (#.+k-get-high-contrast-color+ "HighContrastColor"  :high-contrast-color)
    (#.+k-get-status-area-state+   "StatusAreaState"    :status-area-state)
    (#.+k-get-layout-view-state+   "LayoutViewState"    :layout-view-state)
    (#.+k-get-window-name+         "WindowName"         :window-name)
    (#.+k-get-window-height+       "WindowHeight"       :window-height)
    (#.+k-get-window-left+         "WindowLeft"         :window-left)
    (#.+k-get-window-top+          "WindowTop"          :window-top)
    (#.+k-get-window-visible+      "WindowVisible"      :window-visible)
    (#.+k-get-window-width+        "WindowWidth"        :window-width)
    (#.+k-get-system-nicaddress+   "SystemNICAddress"   :system-nic-address)
    (#.+k-get-system-ip-address+   "SystemIpAddress"    :system-ip-address)
    (#.+k-get-active-field-table-name+ "ActiveFieldTableName" :active-field-table-name)
    (#.+k-get-active-selection-size+   "ActiveSelectionSize" :active-selection-size)
    (#.+k-get-active-selection-start+  "ActiveSelectionStart" :active-selection-start)
    (#.+k-get-application-language+ "ApplicationLanguage" :application-language)
    (#.+k-get-current-host-timestamp+ "CurrentHostTimestamp" :current-host-timestamp)
    (#.+k-get-layout-table-name+    "LayoutTableName"   :layout-table-name)
    (#.+k-get-script-parameter+     "ScriptParameter"   :script-parameter)
    (#.+k-get-current-time-stamp+   "CurrentTimeStamp"  :current-time-stamp)
    (#.+k-get-window-desktop-height+ "WindowDesktopHeight" :window-desktop-height)
    (#.+k-get-window-desktop-width+  "WindowDesktopWidth"  :window-desktop-width)
    (#.+k-get-window-content-height+ "WindowContentHeight" :window-content-height)
    (#.+k-get-window-content-width+  "WindowContentWidth"  :window-content-width)
    (#.+k-get-calculation-repetition-number+
                                   "CalculationRepetitionNumber" :calculation-repetition-number)
    (#.+k-get-current-extended-privileges+
                                   "CurrentExtendedPrivileges" :current-extended-privileges+)
    (#.+k-get-allow-abort-state+   "AllowAbortState"    :allow-abort-state)
    (#.+k-get-error-capture-state+ "ErrorCaptureState"  :error-capture-state)
    (#.+k-get-desktop-path+        "DesktopPath"        :desktop-path)
    (#.+k-get-documents-path+      "DocumentsPath"      :documents-path)
    (#.+k-get-file-maker-path+     "FileMakerPath"      :filemaker-path)
    (#.+k-get-host-ipaddress+      "HostIPAddress"      :host-ip-address)
    (#.+k-get-request-omit-state+  "RequestOmitState"   :request-omit-state)
    (#.+k-get-preferences-path+    "PreferencesPath"    :preferences-path)
    (#.+k-get-record-open-count+   "RecordOpenCount"    :record-open-count)
    (#.+k-get-record-open-state+   "RecordOpenState"    :record-open-state)
    (#.+k-get-script-result+       "ScriptResult"       :script-result)
    (#.+k-get-system-drive+        "SystemDrive"        :system-drive)
    (#.+k-get-text-ruler-visible+  "TextRulerVisible"   :text-ruler-visible)
    (#.+k-get-allow-formatting-bar-state+
                                   "AllowFormattingBarState"
                                   :allow-formatting-bar-state)
    (#.+k-get-use-system-formats-state+
                                   "UseSystemFormatsState"
                                   :use-system-formats-state)
    (#.+k-get-window-zoom-level+   "WindowZoomLevel"    :window-zoom-level)
    (#.+k-get-custom-menu-set-name+ "CustomMenuSetName" :custom-menu-set-name)
    (#.+k-get-active-layout-object-name+
                                   "ActiveLayoutObjectName" :active-layout-object-name)
    (#.+k-get-temporary-path+      "TemporaryPath"      :temporary-path)
    (#.+k-get-host-application-version+
                                   "HostApplicationVersion"
                                   :host-application-version)
    (#.+k-get-trigger-modifier-keys+ "TriggerModifierKeys" :trigger-modifier-keys)
    (#.+k-get-trigger-keystroke+   "TriggerKeystroke"   :trigger-keystroke)
    (#.+k-get-documents-path-listing+ "DocumentsPathListing" :documents-path-listing)
    (#.+k-get-account-privilege-set+ "AccountPrivilegeSet" :account-privilege-set)
    (#.+k-get-account-extended-privileges+
                                   "AccountExtendedPrivileges"
                                   :account-extended-privileges)
    (#.+k-get-quick-find-text+     "QuickFindText"      :quick-find-text)
    (#.+k-get-trigger-current-panel+ "TriggerCurrentPanel" :trigger-current-panel)
    (#.+k-get-trigger-target-panel+  "TriggerTargetPanel" :trigger-target-panel)
    (#.+k-get-window-style+        "WindowStyle"        :window-style)
    (#.+k-get-installed-fmplugins+ "InstalledFMPlugins" :installed-fmplugins)
    (#.+k-get-uuid+                "UUID"               :uuid)
    (#.+k-get-persistent-id+       "PersistentID"       :persistent-id)
    (#.+k-get-connection-state+    "ConnectionState"    :connection-state)
    (#.+k-get-current-time-utcmilliseconds+
                                   "CurrentTimeUTCMilliseconds"
                                   :current-time-utc-milliseconds)
    (#.+k-get-device+              "Device"             :device)
    (#.+k-get-window-orientation+  "WindowOrientation"  :window-orientation)
    (#.+k-get-trigger-gesture-info+ "TriggerGestureInfo" :trigger-gesture-info)
    (#.+k-get-encryption-state+    "EncryptionState"    :encryption-state)
    (#.+k-get-script-animation+    "ScriptAnimation"    :script-animation)
    (#.+k-get-modified-fields+     "ModifiedFields"     :modified-fields)
    (#.+k-get-network-type+        "NetworkType"        :network-type)
    (#.+k-get-connection-attributes+ "ConnectionAttributes" :connection-attributes)
    (#.+k-get-screen-scale-factor+ "ScreenScaleFactor"  :screen-scale-factor)
    (#.+k-get-application-architecture+
                                   "ApplicationArchitecture" :application-architecture)
    (#.+k-get-trigger-external-event+ "TriggerExternalEvent" :trigger-external-event)
    (#.+k-get-touch-keyboard-state+ "TouchKeyboardState" :touch-keyboard-state)
    (#.+k-get-menubar-state+       "MenubarState"       :menubar-state)
    (#.+k-get-region-monitor-events+ "RegionMonitorEvents" :region-monitor-events)
    (#.+k-get-account-group-name+  "AccountGroupName"   :account-group-name)
    (#.+k-get-active-record-number+ "ActiveRecordNumber" :active-record-number)
    (#.+k-get-uuidnumber+          "UUIDNumber"         :uuid-number)
    (#.+k-get-open-data-file-info+ "OpenDataFileInfo"   :open-data-file-info)
    (#.+k-get-account-type+        "AccountType"        :account-type)
    (#.+k-get-page-count+          "PageCount"          :page-count)
    (#.+k-get-system-locale-elements+ "SystemLocaleElements" :system-locale-elements)
    (#.+k-get-file-locale-elements+ "FileLocaleElements" :file-locale-elements)
    (#.+k-get-installed-fmplugins-as-json+
                                   "InstalledFMPluginsAsJSON"
                                   :installed-fmplugins-as-json)
    ;; New to FileMaker Pro 19.4.1
    (#.+k-get-session-identifier+  "SessionIdentifier"  :session-identifier)
    ))

;; These functions map between above functions' values, names and symbols
(defun function-value-to-name (function-value)
  (second (find function-value *get-function-value-map* :key #'first :test #'eql)))
(defun function-symbol-to-value (function-symbol)
  (first (find function-symbol *get-function-value-map* :key #'third :test #'eq)))
(defun function-symbol-to-name (function-symbol)
  (second (find function-symbol *get-function-value-map* :key #'third :test #'eq)))

(defgeneric evaluate-get (function-value &optional result)
  (:documentation "Retrieve the results of a calculation engine's Get() function.
The result is stored in the DATA-OBJECT RESULT which is created if none is provided.
The function returns RESULT."))

(defmethod evaluate-get ((function-value integer)
                         &optional (result (make-data-object)))
  (cond ((<= +k180extn-version+ *fm-version*)
         (let ((err-code
                (fm-expr-env-evaluate-get-function (get-environment)
                                                   function-value
                                                   (pointer result))))
           (unless (zerop err-code)
             (error "Got error code ~A while evaluating \"Get ( ~A )\"."
                    err-code (function-value-to-name function-value)))))
        (t
         (let ((expression (format nil "Get ( ~A )"
                                   (function-value-to-name function-value))))
           (evaluate expression result))))
  result)

(defmethod evaluate-get ((function-value symbol)
                         &optional (result (make-data-object)))
  (when (keywordp function-value)
    (cond ((<= +k180extn-version+ *fm-version*)
           (when-let (value-integer (function-symbol-to-value function-value))
             (evaluate-get value-integer result)))
          (t
           (let ((expression (format nil "Get ( ~A )"
                                   (function-symbol-to-name function-value))))
             (evaluate expression result)))))
  result)

(defgeneric execute-sql (expression file-name column-separator row-separator &optional result)
  (:documentation "Executes \(as with FileMaker's `ExecuteFileSQLTextResult') the
expression EXPRESSION using the column separator COLUMN-SEPARATOR and
the row separator ROW-SEPARATOR \(both of which must be characters).
The result is stored in the DATA-OBJECT RESULT which is created if
none is provided.  The function returns RESULT.

This acts much like the previous deprecated and removed ExecuteSQL method
but uses the more correct/strict SQL syntax of ExecuteFileSQL.
"))

(defmethod execute-sql ((expression text-object) (file-name text-object)
                        column-separator row-separator
                        &optional (result (make-data-object)))
  (let ((err-code (fm-expr-env-execute-file-sqltext-result
                                           (get-environment)
                                           (pointer expression)
                                           (pointer file-name)
                                           *args*
                                           (pointer result)
                                           (char-code column-separator)
                                           (char-code row-separator))))
    (unless (zerop err-code)
      (error "Got error code ~A while executing SQL ~S with column separator ~S and row separator ~S."
             err-code (as-string expression) column-separator row-separator)))
  result)

(defmethod execute-sql ((expression string) (file-name text-object)
                        column-separator row-separator
                        &optional (result (make-data-object)))
  (execute-sql (make-text-object expression) column-separator row-separator result))

(defun set-value (value &key (target *results*) result-type)
  "Sets the contents of the DATA-OBJECT TARGET to VALUE.  If
TARGET is not provided, the return value of the currently
executing plug-in function is set.  RESULT-TYPE can be one
of :BOOLEAN, :DATE, :TIME, :TIMESTAMP, :UNIVERSAL-TIME describing
the intended FileMaker type of VALUE.  RESULT-TYPE can also be
NIL in which case the function tries to do the right thing
depending on the Lisp type of VALUE.  Finally, RESULT-TYPE can
be :VOID which means that this function does nothing."
  (ecase result-type
    (:void)
    (:boolean
     (with-fix-pt (fix-pt-ptr (if value 1 0))
       (fm-data-set-as-number (pointer target) fix-pt-ptr +k-dtnumber+)))
    (:date
     (unless (typep value 'date-time-object)
       (error "Value ~S should have been a DATE-TIME-OBJECT for result type :DATE."
              value))
     (fm-data-set-as-date (pointer target) (pointer value) +k-dtdate+))
    (:time
     (unless (typep value 'date-time-object)
       (error "Value ~S should have been a DATE-TIME-OBJECT for result type :TIME."
              value))
     (fm-data-set-as-time (pointer target) (pointer value) +k-dttime+))
    (:timestamp
     (unless (typep value 'date-time-object)
       (error "Value ~S should have been a DATE-TIME-OBJECT for result type :TIMESTAMP."
              value))
     (fm-data-set-as-time-stamp (pointer target) (pointer value) +k-dttime-stamp+))
    (:universal-time
     (let ((date-time (make-date-time-object :universal-time value)))
       (fm-data-set-as-time-stamp (pointer target) (pointer date-time) +k-dttime-stamp+)))
    ((nil)
     (typecase value
       (data-object)
       (string
        (with-text (text-ptr value)
          (fm-data-set-as-text* (pointer target) text-ptr)))
       ((or integer float)
        (with-fix-pt (fix-pt-ptr value)
          (fm-data-set-as-number (pointer target) fix-pt-ptr +k-dtnumber+)))
       (text-object
        (fm-data-set-as-text* (pointer target) (pointer value)))
       (fix-pt-object
        (fm-data-set-as-number (pointer target) (pointer value) +k-dtnumber+))
       (binary-data-object
        (fm-data-set-binary-data (pointer target) (pointer value) t))
       (otherwise
        ;; boolean
        (with-fix-pt (fix-pt-ptr (if value 1 0))
          (fm-data-set-as-number (pointer target) fix-pt-ptr +k-dtnumber+))))))
  target)

(define-foreign-funcallable fmx-start-script
    ((file-name (:pointer :void))
     (script-name (:pointer :void))
     (control (:unsigned :char))
     (parameter (:pointer :void)))
  :documentation "Starts the FileMaker script with the name
SCRIPT-NAME in the file FILE-NAME.  See the FileMaker documentation
for the meaning of the CONTROL and PARAMETER arguments."
  :result-type :short)

(defgeneric start-script (file-name script-name &key control parameter)
  (:documentation "Starts the FileMaker script with the name
SCRIPT-NAME in the file FILE-NAME.  PARAMETER can be a
DATA-OBJECT or any other object that can be converted
automatically to a DATA-OBJECT with SET-VALUE.  See the FileMaker
documentation for the meaning of the CONTROL and PARAMETER
arguments."))

(defmethod start-script ((file-name text-object) (script-name text-object)
                         &key (control +k-fmxt-pause+) parameter)
  (when (and parameter (not (typep parameter 'data-object)))
    (let ((data-object (make-data-object)))
      (set-value parameter :target data-object)
      (setq parameter data-object)))
  (unless (zerop
           (fmx-start-script (c-start-script) (pointer file-name) (pointer script-name)
                             control (if parameter (pointer parameter) *null-pointer*)))
    (error "An error occurred while trying to execute script ~S in ~S."
           (as-string script-name) (as-string file-name)))
  (values))

(defmethod start-script ((file-name string) script-name
                         &key (control +k-fmxt-pause+) parameter)
  (start-script (make-text-object file-name) script-name
                :control control :parameter parameter))

(defmethod start-script (file-name (script-name string)
                         &key (control +k-fmxt-pause+) parameter)
  (start-script file-name (make-text-object script-name)
                :control control :parameter parameter))

(defun enlist-plugin-function (prototype
                               documentation ; new
                               c-name min-args max-args flags)
  "Adds a new plug-in function with the corresponding parameters
to the list *PLUGIN-FUNCTIONS*."
  (pushnew (list (next-function-id)
                 prototype
                 documentation ; new
                 c-name
                 min-args
                 max-args
                 flags)
           *plugin-functions*
           :key (lambda (tuple)
                  (function-name (second tuple)))
           :test #'string=))

(defun enlist-plugin-script-step (name definition documentation c-name flags)
  "Adds a new plug-in script step with the corresponding parameters
to the list *PLUGIN-SCRIPT-STEPS*."
  (pushnew (list (next-function-id)
                 name
                 definition
                 documentation
                 c-name
                 flags)
           *plugin-script-steps*
           :key (lambda (tuple) (second tuple))
           :test #'string=))

(defun nth-arg (n &optional type)
  "Returns the Nth argument \(if there is one) of the currently
executing plug-in function.  TYPE determines how the argument
should be returned and must be one of :TEXT \(for a
TEXT-OBJECT), :STRING \(for a Lisp string), :FIX-PT \(a
FIX-PT-OBJECT), :INTEGER \(a Lisp integer), :FLOAT \(a Lisp
float), :BOOLEAN \(a Lisp boolean), :DATE, :TIME, :TIMESTAMP
\(DATE-TIME-OBJECTs), :UNIVERSAL-TIME \(a Lisp universal
time), :BINARY-DATA \(a BINARY-DATA-OBJECT), or NIL \(the
DATA-OBJECT itself)."
  (when (< n (fm-data-vect-size *args*))
    (case type
      (:text (make-instance 'text-object
                            :pointer (fm-data-vect-at-as-text *args* n)
                            :do-not-delete t))
      (:string (fm-text-get-string (fm-data-vect-at-as-text *args* n)))
      (:fix-pt (make-instance 'fix-pt-object
                             :pointer (fm-data-vect-at-as-number *args* n)
                             :do-not-delete t))
      (:integer (fm-fix-pt-as-long (fm-data-vect-at-as-number *args* n)))
      (:float (fm-fix-pt-as-float (fm-data-vect-at-as-number *args* n)))
      (:boolean (fm-data-vect-at-as-boolean *args* n))
      (:date (make-instance 'date-time-object
                            :pointer (fm-data-vect-at-as-date *args* n)
                            :do-not-delete t))
      (:time (make-instance 'date-time-object
                            :pointer (fm-data-vect-at-as-time *args* n)
                            :do-not-delete t))
      (:timestamp (make-instance 'date-time-object
                                 :pointer (fm-data-vect-at-as-time-stamp *args* n)
                                 :do-not-delete t))
      (:universal-time (as-universal-time
                        (make-instance 'date-time-object
                                       :pointer (fm-data-vect-at-as-time-stamp *args* n)
                                       :do-not-delete t)))
      (:binary-data (make-instance 'binary-data-object
                                   :pointer (fm-data-vect-at-as-binary-data *args* n)
                                   :do-not-delete t))
      ((nil) (make-instance 'data-object
                            :pointer (fm-data-vect-at *args* n)
                            :do-not-delete t)))))

(defun create-bindings (lambda-list)
  "Accepts a lambda list as for DEFINE-PLUGIN-FUNCTION, checks
it, and returns a list of corresponding LET bindings."
  (let ((counter 0)
        (state :required)
        (bindings nil)
        (min-args 0)
        (max-args nil))
    (dolist (thing lambda-list)
      (case thing
        (&optional
         (unless (eq state :required)
           (error "Unexpected &OPTIONAL in lambda list."))
         (setq state :optional))
        (&rest
         (unless (member state '(:required :optional))
           (error "Unexpected &REST in lambda list."))
         (setq state :rest))
        (otherwise
         (when (eq state :done)
           (error "Only one parameter may follow &REST."))
         (when (atom thing)
           (unless (symbolp thing)
             (error "Expected symbol in lambda list but got ~S." thing))
           (setq thing (list thing nil)))
         (unless (and (or (eql 2 (list-length thing))
                          (and (eq state :optional)
                               (eql 3 (list-length thing))))
                      (symbolp (first thing))
                      (member (second thing) '(:text :string :fix-pt :integer :float :boolean :date
                                               :time :timestamp :universal-time :binary-data nil)))
           (error "Illegal parameter specifier ~S." thing))
         (push (list (first thing)
                     (case state
                       (:rest
                        (with-unique-names (i)
                          `(loop for ,i from ,counter below (fm-data-vect-size *args*)
                                 collect (nth-arg ,i ,(second thing)))))
                       (otherwise
                        ;; note that (THIRD THING) is NIL for required arguments
                        `(or (nth-arg ,counter ,(second thing)) ,(third thing)))))
               bindings)
         (incf counter)
         (when (eq state :required)
           (incf min-args))
         (when (eq state :rest)
           (setq state :done)))))
    (when (eq state :rest)
      (error "No parameter following &REST."))
    (unless (eq state :done)
      (setq max-args counter))
    (values (nreverse bindings) min-args max-args)))

(defmacro define-plugin-function (description lambda-list &body body)
  "Defines a plug-in function.  DESCRIPTION is either a string
with the function prototype as it should be shown by FileMaker or
a list where the first element is this prototype string followed
by a plist.

The plist can have the properties :MAX-ARGS, :FLAGS,
and :RESULT-TYPE.  MAX-ARGS is the maximal number of arguments
for the function.  It will only be used if there's a &REST
parameter in the lambda list.  RESULT-TYPE will be interpreted as
by SET-VALUE.  FLAGS is a boolean combination of flags describing
the behaviour of the function - see the FileMaker documentation.

LAMBDA-LIST is like a simplified version of a Lisp lambda list
where only &OPTIONAL and &REST are allowed.  Each parameter is
either a symbol or a pair \(NAME TYPE) where TYPE is interpreted
as by NTH-ARG.  Optional parameters can also look like \(NAME
TYPE DEFAULT-VALUE)."
  ;; we want to always treat DESCRIPTION like a list
  (when (atom description)
    (setq description (list description)))
  (let ((prototype (first description))
        (result-type (getf (rest description) :result-type))
        (callable-name (next-callable-name))
        documentation) ; new
    ;; if the first form of body is a literal string, treat it as documentation
    (when (stringp (first body))
      (setq documentation (first body))
      (setq body (rest body)))
    (multiple-value-bind (bindings min-args max-args)
        (create-bindings lambda-list)
      (with-unique-names (func-id result results cond error-occurred)
        `(progn
           ;; create the actual C stub which will be called by FileMaker
           (define-foreign-callable (,callable-name :result-type :short
                                                    :calling-convention :stdcall)
               ((,func-id :short)
                ;; bind special variables
                (*environment* (:pointer :void))
                (*args* (:pointer :void))
                (,results (:pointer :void)))
             (declare (ignore ,func-id))
             (catch ',error-occurred
               (handler-bind
                   ((error (lambda (,cond)
                             (maybe-log-error ,cond ,prototype)
                             ;; return -1 to FileMaker in case of an error
                             (throw ',error-occurred -1))))
                 (let* ((*results* (make-instance 'data-object
                                                  :pointer ,results
                                                  :do-not-delete t))
                        (*errno* 0)
                        ,@bindings
                        (,result (block nil ,@body)))
                   ,@(unless (eq result-type :void)
                       ;; set return value
                       `((set-value ,result :result-type ,result-type)))
                   ;; return 0 for "no error"
                   (if (<= +k160extn-version+ *fm-version*) *errno* 0)))))
           ;; the name of the callable must be kept if the delivery
           ;; level is 5
           (push ',callable-name *symbols-to-keep*)
           ;; enlist function so it will be registered when the
           ;; plug-in is initialized
           (enlist-plugin-function ,prototype
                                   ,documentation ; new
                                   ',callable-name
                                   ,min-args
                                   ,(or max-args (getf (rest description) :max-args))
                                   ,(getf (rest description) :flags)))))))

;; New to FileMaker Pro 16 (API VERSION 57) and later
;; Plug-in XML UI definition
;;
;; This XML is the text that will be passed to the scriptStepDefinition parameter
;; of the RegisterScriptStep function. Up to ten script parameters can be specified
;; in addition to the optional target parameter. All the parameters are defined with
;; <Parameter> tags in a <PluginStep> grouping.
;;
;; The attributes for a <Parameter> tag include:
;;
;;  * Type - If not one of the following four types, the parameter is ignored.
;;
;;    1. Calc - A standard Specify button that brings up the calculation dialog. When
;;              the script step is executed, the calculation will be evaluated and its
;;              results passed to the plug-in.
;;    2. Bool - Simple check box that returns the value of 0 or 1.
;;    3. List - A static drop-down or pop-up list in which the ID of the item selected
;;              is returned. The size limit of this list is limited by the capabilities
;;              of the UI widgets used to display it. A List type parameter expects to
;;              contain <Value> tags as specified below.
;;    4. Target - Will include a specify button that uses the new Insert From Target field
;;              targeting dialog that allows a developer to put the results of a script
;;              step into a field (whether or not it is on a layout), into a variable, or
;;              insert into the current active field on a layout. If no Target is defined
;;              then the result Data object is ignored. If there are multiple Target
;;              definitions, only the first one will be honored.
;;
;;  * ID - A value in the range of 0 to 9, which is used as an index into the
;;         DataVect& parms object for the plug-in to retrieve the value of the parameter.
;;         Indexes that are not in range or duplicated will cause the parameter to be
;;         ignored. A parameter of type Target ignores this attribute if specified.
;;
;;  * Label - The name of parameter or control that is displayed in the UI.
;;
;;  * DataType - Only used by the Calc and Target parameter types. If not specified or not
;;               one of the six data types, the type Text will be used.
;;    1. Text
;;    2. Number
;;    3. Date
;;    4. Time
;;    5. Timestamp
;;    6. Container 
;;
;;  * ShowInline - Value is either true or false. If defined and true, will cause the
;;          parameter to show up inlined with the script step in the Script Workspace.
;;
;;  * Default - Either the numeric index of the default list item or the true/false
;;              value for a bool item. Ignored for calc and target parameters.
;;
;; Parameters of type List are expected to contain <Value> tags whose values are used to
;; construct the drop-down or pop-up list. The ID of a value starts at zero, but a
;; specific ID can be given to a value by defining an "ID" attribute. If later values do
;; not have an "ID" attribute, the ID will be set to the previous value's ID plus one.
;;
;; Sample XML description:
;;
;;  <PluginStep>
;;    <Parameter ID="0" Type="Calc" DataType="text" ShowInline="true" Label="Mood"/>
;;    <Parameter ID="1" Type="List" ShowInline="true" Label="Color">
;;      <Value ID="0">Red</Value>
;;      <Value ID="1">Green</Value>
;;      <Value ID="2">Blue</Value>
;;    </Parameter>
;;    <Parameter ID="2" Type="Bool" Label="Beep when happy"/>
;;  </PluginStep>
;;
(defun create-script-step-bindings (lambda-list)
  "Accepts a lambda list as for DEFINE-PLUGIN-FUNCTION, checks
it, and returns a list of corresponding LET bindings."
  (let ((counter 0)
        (bindings nil)
        (parameters nil))
    (dolist (thing lambda-list)
      (when (atom thing)
        (unless (symbolp thing)
          (error "Expected symbol in lambda list but got ~S." thing))
        (setq thing (list thing :text '(:type :calc))))
      (unless (and (<= 2 (list-length thing))
                   (symbolp (first thing))
                   (member (second thing) '(:text :string :integer :float :boolean :date
                                            :time :timestamp :universal-time :binary-data nil)))
        (error "Illegal parameter specifier ~S." thing))
      (let* ((variable (first thing)) ; symbolp
             (type (second thing))    ; one of the above types, including nil
             (alist (cddr thing))
             (ptype (second (assoc :type alist))))
        ;; :target parameter does not enter into bindings
        (unless (eq ptype :target)
          (push `(,variable (nth-arg ,counter ,type)) bindings))
        (push `("Parameter" (,@(unless (eq ptype :target)
                                 `(("ID" ,(format nil "~A" counter))))
                             ("Type" ,(string-downcase (symbol-name ptype)))
                             ,@(when (member ptype '(:target :calc))
                                 `(("DataType" ,(case type
                                                  ((nil :text :string)     "Text")
                                                  ((:integer :float)       "Number")
                                                  (:date                   "Date")
                                                  ((:time :universal-time) "Time")
                                                  ((:timestamp             "Timestamp"))
                                                  (:binary-data            "Container")))))
                             ,@(when-let (label (second (assoc :label alist)))
                                 `(("Label" ,label)))
                             ,@(when-let (inlinep (second (assoc :inline alist)))
                                 `(("ShowInline" ,(if inlinep "true" "false"))))
                             ,@(when (member ptype '(:list :bool))
                                 (when-let (default (second (assoc :default alist)))
                                   `(("Default" ,(cond ((eq ptype :bool)
                                                        (if default "true" "false"))
                                                       (t
                                                        (format nil "~A" default))))))))
                ,@(when (eq ptype :list)
                    (let ((contents (second (assoc :contents alist)))
                           (acc nil))
                       (loop for (id value) in contents do
                         (push `("Value" (("ID" ,(format nil "~A" id))) ,value) acc))
                       (nreverse acc))))
              parameters)
        (unless (eq ptype :target)
          (incf counter))))
    (values (nreverse bindings)
            (append (list "PluginStep" nil) (nreverse parameters)))))

;; New to FileMaker Pro 16 (API VERSION 57) and later
;; Dynamic Registration of Script Steps
(defmacro define-plugin-script-step (description lambda-list &body body)
  "Defines a plug-in script step (new to FileMaker Pro 16 and later).

DESCRIPTION is either the name of the script step as it should appear in the
scripting workspace, or a list where the first element is this name string followed
by a plist.

The plist can have the properties :FLAGS and :RESULT-TYPE.
RESULT-TYPE will be interpreted as by SET-VALUE.
FLAGS is a boolean combination of flags describing
the behaviour of the function - see the FileMaker documentation.

DEFINITION is the XML definition of what parameters should be displayed for
the script step. (TODO)

LAMBDA-LIST is like a simplified version of a Lisp lambda list
where only &OPTIONAL and &REST are allowed.  Each parameter is
either a symbol or a pair \(NAME TYPE) where TYPE is interpreted
as by NTH-ARG.  Optional parameters can also look like \(NAME
TYPE DEFAULT-VALUE)."
  ;; we want to always treat DESCRIPTION like a list
  (when (atom description)
    (setq description (list description)))
  (let ((script-step-name (first description))
        (result-type (getf (rest description) :result-type))
        (callable-name (next-callable-name))
        documentation)
    ;; if the first form of body is a literal string, treat it as documentation
    (when (stringp (first body))
      (setq documentation (first body) body (rest body)))
    (multiple-value-bind (bindings parameters)
        (create-script-step-bindings lambda-list)
      (with-unique-names (func-id result results cond error-occurred)
        `(progn
           ;; create the actual C stub which will be called by FileMaker
           (define-foreign-callable (,callable-name :result-type :short
                                                    :calling-convention :stdcall)
               ((,func-id :short)
                ;; bind special variables
                (*environment* (:pointer :void))
                (*args* (:pointer :void))
                (,results (:pointer :void)))
             (declare (ignore ,func-id))
             (catch ',error-occurred
               (handler-bind
                   ((error (lambda (,cond)
                             (maybe-log-error ,cond ,script-step-name)
                             ;; return -1 to FileMaker in case of an error
                             (throw ',error-occurred -1))))
                 (let* ((*results* (make-instance 'data-object
                                                  :pointer ,results
                                                  :do-not-delete t))
                        (*errno* 0)
                        ,@bindings
                        (,result (block nil ,@body)))
                   ,@(unless (eq result-type :void)
                       ;; set return value
                       `((set-value ,result :result-type ,result-type)))
                   ;; return 0 for "no error"
                   (if (<= +k160extn-version+ *fm-version*) *errno* 0)))))
           ;; the name of the callable must be kept if the delivery
           ;; level is 5
           (push ',callable-name *symbols-to-keep*)
           ;; enlist function so it will be registered when the
           ;; plug-in is initialized
           (enlist-plugin-script-step ,script-step-name
                                      ,(write-xml parameters)
                                      ,documentation
                                      ',callable-name
                                      ,(getf (rest description) :flags)))))))
