;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2006-2008, Dr. Jens Teich and Dr. Edmund Weitz.  All rights reserved.
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

(in-package :cl-user)

(defvar *regex-plugin-version* '(0 3 1)
  "Version number.  Should be a list of three non-negative integers,
e.g. (4 2 1) would correspond to version \"4.2.1\".")

;; we export the name so we can import it later
(export '*regex-plugin-version*)

(defpackage :fm-plugin-regex-system
  (:use :cl :asdf))

(in-package :fm-plugin-regex-system)

(defsystem fm-plugin-regex
  :name "RegexPlugin"
  :description "A Plug-In which enables FileMaker users to work with regular expressions."
  :author "Jens Teich and Edi Weitz"
  :license "BSD"
  :version #.(format nil "~{~A~^.~}" cl-user:*regex-plugin-version*)
  :serial t
  :components ((:module "regex-plugin"
                        :serial t
                        :components ((:file "packages")
                                     (:file "specials")
                                     (:file "utils")
                                     (:file "configuration")
                                     (:file "functions")
                                     (:file "init"))))
  ;; note: every system which creates a FileMaker plug-in must use the
  ;; :FM-PLUGIN-TOOLS system
  :depends-on (:fm-plugin-tools
               :cl-ppcre-unicode))
