;;; unity-launcher-api.el --- Unity Launcher API -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Created: July 16, 2025
;; Version: 0.0.1
;; Keywords: lisp
;; Homepage: https://github.com/hron/unity-launcher-api.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Control taskbar entry of Emacs to have count badge, progress and
;;  urgent status
;;
;;; Code:

(require 'dbus)

(defun unity-launcher-api-update (api-uri properties-alist)
  "Send D-Bus signal to com.canonical.Unity.LauncherEntry.

API-URI: a string on the form application://$desktop_file_id.  The
desktop file id of an application is defined to be the basename of the
application's .desktop-file including the extension.  So taking Emacs as
an example it would be application://emacs.desktop

PROPERTIES-ALIST: The properties to set on the launcher icon.  Valid
properties are:

`count` (integer) : A number to display on the launcher icon.  You must
also set the `count-visible` property to true in order for this to show.

`progress` (float) : A double precision floating point number between 0
and 1. This will be rendered as a progress bar or similar on the
launcher icon.  You must also set the `progress-visible` property to
true in order for this to show.

`urgent` (boolean) : Tells the launcher to get the users attention

`quicklist` (type signature s) : The object path to a DbusmenuServer
instance on the emitting process.  An empty string denotes that the
quicklist has been unset.  This also explains why we use signature s and
not o. The empty string is not a valid object path.

`count-visible` (boolean) : Determines whether the `count` is visible

`progress-visible` (boolean) : Determines whether the `progress` is
visible

`updating` (boolean) : Tells the launcher that the application is being
updated, to inform the user."
  (dbus-send-signal
   :session
   nil
   "/"
   "com.canonical.Unity.LauncherEntry"
   "Update"
   api-uri
   (unity-launcher-api--build-dbus-args properties-alist)))

(defun unity-launcher-api--build-dbus-args (properties-alist)
  "Convert PROPERTIES-ALIST to args for dbus-send-signal."
  (seq-map
   (lambda (prop)
     (let* ((key (symbol-name (car prop)))
            (value (cdr prop))
            (value-type (cond
                         ((memq value '(t nil)) :boolean)
                         ((natnump value) :uint32)
                         ((fixnump value) :int32)
                         ((floatp value) :double)
                         ((stringp value) :string)
                         (t
                          (signal 'wrong-type-argument (list "Value type invalid" value)))))
            (value `(:variant ,value-type ,value)))
       `(:dict-entry ,key ,value)))
   properties-alist))

(provide 'unity-launcher-api)
;;; unity-launcher-api.el ends here
