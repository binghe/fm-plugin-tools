;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2018,2022, Chun Tian (binghe).  All rights reserved.

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

(asdf:defsystem fm-plugin-tools
  :description "A toolkit for FileMaker plug-in development in Common Lisp"
  :author "Dr. Edmund Weitz (Edi Weitz)"
  :maintainer "Chun Tian (binghe)"
  :license "BSD"
  ;; we increase this version even if only the parser has changed
  :version "0.4.1"
  :serial t
  :components ((:file "packages")
               (:file "specials")
               (:file "utils")
               (:file "fli") ; not included in distribution, see docs
               (:file "fm-objects")
               (:file "locale-objects")
               (:file "color-objects")
               (:file "style-objects")
               (:file "text-objects")
               (:file "fix-pt-objects")
               (:file "date-time-objects")
               (:file "binary-data-objects")
               (:file "data-objects")
               (:file "functions")
               (:file "main"))
  ;; cxml is for generating XML code when registering script steps (FMP 16+)
  :depends-on (:cxml/xml))
