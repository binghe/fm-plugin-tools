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

(defun show-cache-size ()
  "Returns a string showing the current cache size."
  (format nil "~D validation~:P cached" (cache-size)))

(capi:define-interface config-interface ()
  ()
  (:panes
   (cache-state-button
    capi:check-button
    :text "Cache validation results"
    :selected *cache-validation-p*
    :callback-type :none
    :selection-callback (lambda ()
                          (setq *cache-validation-p* t)
                          (store-config-values))
    :retract-callback (lambda ()
                        (setq *cache-validation-p* nil)
                        (store-config-values))
    :help-key "Whether validation results should be cached to improve performance.
Un-check this if you're frequently changing invoice data.")
   (cache-empty-button
    capi:push-button
    :reader config-interface-cache-empty-button
    :title ""
    :title-position :right
    :text "Clear cache"
    :enabled *cache-validation-p*
    :callback-type :none
    :callback (lambda ()
                (clear-validation-cache)
                (setf (capi:titled-object-title cache-empty-button)
                      (show-cache-size))
                (gc-generation #+:lispworks-32bit 3
                               #+:lispworks-64bit :blocking-gen-num))
    :help-key "Removes all cached validation results.")
   (info-pane
    capi:title-pane
    :text (format nil "X-Rechnung Version: ~A~%PDF/A Level: ~A"
                  *xrechnung-version* *pdf-a-level*))
   (ok-button
    capi:push-button
    :reader config-interface-ok-button
    :default-p t
    :text "   OK   "
    :callback-type :none
    :callback (lambda ()
                (capi:abort-dialog))
    :help-key "Close this dialog.")
   (description-pane
    capi:title-pane
    :text "(Move your mouse over the buttons for more information.)"))
  (:layouts
   (dummy-layout
    capi:column-layout
    '(" ")
    :visible-max-height '(:character 1)
    :visible-max-width :screen-width)
   (ok-layout
    capi:row-layout
    '(dummy-layout ok-button))
   (main-layout
    capi:column-layout
    '(info-pane nil cache-state-button cache-empty-button nil description-pane nil ok-layout)
    :gap 10))
  (:documentation "The interface class used to create the
configuration dialog for this plug-in.")
  (:default-initargs
   :layout 'main-layout
   :auto-menus nil
   :title *plugin-name*
   :internal-border 10
   :create-callback (lambda (interface)
                      (setf (capi:titled-object-title
                             (config-interface-cache-empty-button interface))
                            (show-cache-size)))
   :help-callback (lambda (interface pane type key)
                    (declare (ignore interface pane))
                    (when (and (stringp key)
                               (eq type :tooltip))
                      key))
   :top-level-hook #'top-level-hook))

(defun handle-configuration ()
  "The function to handle plug-in configuration.
Called when the user presses the 'Configure' button in FileMaker's
preferences dialog."
  (let ((config-interface (make-instance 'config-interface)))
    (setf (capi:pane-initial-focus config-interface)
          (config-interface-ok-button config-interface))
    (capi:display-dialog config-interface)))
