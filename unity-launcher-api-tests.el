;;; unity-launcher-api-tests.el --- Tests for Unity Launcher API -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Created: July 16, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/hron/unity-launcher-api.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'unity-launcher-api)
(require 'ert)

(ert-deftest unity-launcher-api-tests-build-dbus-args ()
  (should
   (equal (unity-launcher-api--build-dbus-args '((urgent . t) (count . 42)))
          `((:dict-entry "urgent" (:variant :boolean t))
            (:dict-entry "count" (:variant :uint32 42))))))

(provide 'unity-launcher-api-tests)
;;; unity-launcher-api-tests.el ends here
