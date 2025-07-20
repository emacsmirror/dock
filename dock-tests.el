;;; dock-tests.el --- Tests for Unity Launcher API -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Created: July 16, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/hron/dock.el
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: MIT
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'dock)
(require 'ert)

(ert-deftest dock-tests-build-dbus-args ()
  (should
   (equal (dock--build-dbus-args :urgent t)
          `((:dict-entry "urgent" (:variant :boolean t)))))
  (should
   (equal (dock--build-dbus-args :count 42 :count-visible t)
          `((:dict-entry "count-visible" (:variant :boolean t))
            (:dict-entry "count" (:variant :uint32 42)))))
  (should
   (equal (dock--build-dbus-args :progress 0.42 :progress-visible t)
          `((:dict-entry "progress-visible" (:variant :boolean t))
            (:dict-entry "progress" (:variant :double 0.42)))))
  (should
   (equal (dock--build-dbus-args :updating t)
          `((:dict-entry "updating" (:variant :boolean t))))))

;;; dock-tests.el ends here
