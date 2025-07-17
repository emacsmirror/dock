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

(defun dock-update (&rest params)
  "Send D-Bus signal to Dock with update about Emacs.
Various PARAMS can be set:

 :bus              The D-Bus bus, if different from `:session'.
 :count            A number to display on the launcher icon.  You must
                   also set the `count-visible` property to true in order
                   for this to show.
 :progress         A double precision floating point number between 0 and
                   1. This will be rendered as a progress bar or similar
                   on the launcher icon.  You must also set the
                   `progress-visible` property to true in order for this
                   to show.
 :urgent           Tells the launcher to get the users attention
 :quicklist        The object path to a DbusmenuServer instance on the
                   emitting process.  An empty string denotes that the
                   quicklist has been unset.  This also explains why we
                   use signature s and not o. The empty string is not a
                   valid object path.
 :count-visible    Determines whether the `count` is visible
 :progress-visible Determines whether the `progress` is visible
 :updating         Tells the launcher that the application is being updated, to
                   inform the user."
  (dbus-send-signal
   (or (plist-get params :bus) :session)
   nil
   "/"
   "com.canonical.Unity.LauncherEntry"
   "Update"
   (concat "application://" dock-desktop-file)
   (funcall #'dock--build-dbus-args params)))

(defun dock--build-dbus-args (&rest params)
  "Convert the function arguments to arguments for `dbus-send-signal'.

PARAMS are converted to corresponding arguments `dbus-send-signal'
requires.  This is a helper for `dock-update', see its documentation for
arguments meaning."
  (let (args)

    (when (plist-member params :urgent)
      (let ((value (plist-get params :urgent) ))
        (push `(:dict-entry "urgent" (:variant :boolean ,value)) args)))

    (when-let (count (plist-get params :count))
      (push `(:dict-entry "count" (:variant :uint32 ,count)) args))

    (when (plist-member params :count-visible)
      (let ((value (plist-get params :count-visible) ))
        (push `(:dict-entry "count-visible" (:variant :boolean ,value)) args)))

    (when-let (progress (plist-get params :progress))
      (push `(:dict-entry "progress" (:variant :double ,progress)) args))

    (when (plist-member params :progress-visible)
      (let ((value (plist-get params :progress-visible)))
        (push `(:dict-entry "progress-visible" (:variant :boolean ,value)) args)))

    (when (plist-member params :updating)
      (let ((value (plist-get params :updating)))
        (push `(:dict-entry "updating" (:variant :boolean ,value)) args)))

    args))

(provide 'dock)
;;; dock.el ends here
