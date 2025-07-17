;;; dock.el --- Unity Launcher API -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Created: July 16, 2025
;; Version: 0.0.1
;; Keywords: lisp
;; Homepage: https://github.com/hron/dock.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Integrate desktop environment's taskbar/dock with Emacs.
;;
;;; Code:

(require 'dbus)

(defgroup dock nil
  "Integrate desktop environment's taskbar/dock with Emacs."
  :link '(url-link :tag "Website" "https://github.com/hron/dock.el")
  :link '(emacs-library-link :tag "Library Source" "dock.el")
  :group 'convenience
  :group 'environment
  :group 'frames
  :prefix "dock-")

(defcustom dock-desktop-file "emacs.desktop"
  "The desktop file id of Emacs.

This is used when sending D-Bus messages to pinpoint the taskbar entry
for updating the properties.  Usually, this is just `emacs.desktop', but
in case Emacs is used as a dedicated window for applications like
`org-mode', with separate desktop file, you might want to set it
accordingly to match such entry on the taskbar."
  :group 'dock
  :type 'string)

(defun dock-update (properties-alist)
  "Send D-Bus signal to com.canonical.Unity.LauncherEntry.

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
   (concat "application://" dock-desktop-file)
   (dock--build-dbus-args properties-alist)))

(defun dock--build-dbus-args (properties-alist)
  "Convert PROPERTIES-ALIST to args for `dbus-send-signal'."
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

(provide 'dock)
;;; dock.el ends here
