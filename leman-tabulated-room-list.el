;;; leman-tabulated-room-list.el --- Leman tabulated room list buffer    -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a room list buffer with `tabulated-list-mode'.

;; NOTE: It doesn't appear that there is a way to get the number of
;; members in a room other than by retrieving the list of members and
;; counting them.  For a large room (e.g. the Spacemacs Gitter room or
;; #debian:matrix.org), that means thousands of users, none of the
;; details of which we care about.  So it seems impractical to know
;; the number of members when using lazy-loading.  So I guess we just
;; won't show the number of members.

;; TODO: (Or maybe there is, see m.joined_member_count).

;; NOTE: The tabulated-list API is awkward here.  When the
;; `tabulated-list-format' is changed, we have to make the change in 4
;; or 5 other places, and if one forgets to, bugs with non-obvious
;; causes happen.  I think library using EIEIO or structs would be
;; very helpful.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'tabulated-list)

(require 'leman)

;;;; Variables

(declare-function leman-notify-switch-to-mentions-buffer "leman-notify")
(declare-function leman-notify-switch-to-notifications-buffer "leman-notify")
(defvar leman-tabulated-room-list-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "g") #'tabulated-list-revert)
    ;; (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "SPC") #'leman-tabulated-room-list-next-unread)
    (define-key map (kbd "M-g M-m") #'leman-notify-switch-to-mentions-buffer)
    (define-key map (kbd "M-g M-n") #'leman-notify-switch-to-notifications-buffer)
    ;; (define-key map (kbd "S") #'tabulated-list-sort)
    map))

(defvar leman-tabulated-room-list-timestamp-colors nil
  "List of colors used for timestamps.
Set automatically when `leman-tabulated-room-list-mode' is activated.")

(defvar leman-sessions)

;;;; Customization

(defgroup leman-tabulated-room-list-faces nil
  "Faces for tabulated room list buffers."
  :group 'leman-tabulated-room-list
  :group 'leman-faces)

(defgroup leman-tabulated-room-list nil
  "Options for tabulated room list buffers."
  :group 'leman)

(defcustom leman-tabulated-room-list-auto-update t
  "Automatically update the room list buffer."
  :type 'boolean)

(defcustom leman-tabulated-room-list-avatars (display-images-p)
  "Show room avatars in the room list."
  :type 'boolean)

(defcustom leman-tabulated-room-list-simplify-timestamps t
  "Only show the largest unit of time in a timestamp.
For example, \"1h54m3s\" becomes \"1h\"."
  :type 'boolean)

;;;;; Faces

(defface leman-tabulated-room-list-name
  '((t (:inherit (font-lock-function-name-face button))))
  "Non-direct rooms."
  :group 'leman-tabulated-room-list-faces)

(defface leman-tabulated-room-list-direct
  ;; In case `font-lock-constant-face' is bold, we set the weight to normal, so it can be
  ;; made bold for unread rooms only.
  '((t (:weight normal :inherit (font-lock-constant-face leman-tabulated-room-list-name))))
  "Direct rooms."
  :group 'leman-tabulated-room-list-faces)

(defface leman-tabulated-room-list-invited
  '((t (:inherit (italic leman-tabulated-room-list-name))))
  "Invited rooms."
  :group 'leman-tabulated-room-list-faces)

(defface leman-tabulated-room-list-left
  '((t (:strike-through t :inherit leman-tabulated-room-list-name)))
  "Left rooms."
  :group 'leman-tabulated-room-list-faces)

(defface leman-tabulated-room-list-unread
  '((t (:inherit (bold leman-tabulated-room-list-name))))
  "Unread rooms."
  :group 'leman-tabulated-room-list-faces)

(defface leman-tabulated-room-list-favourite '((t (:inherit (font-lock-doc-face leman-tabulated-room-list-name))))
  "Favourite rooms."
  :group 'leman-tabulated-room-list-faces)

(defface leman-tabulated-room-list-low-priority '((t (:inherit (font-lock-comment-face leman-tabulated-room-list-name))))
  "Low-priority rooms."
  :group 'leman-tabulated-room-list-faces)

(defface leman-tabulated-room-list-recent
  '((t (:inherit font-lock-warning-face)))
  "Latest timestamp of recently updated rooms.
The foreground color is used to generate a gradient of colors
from recent to non-recent for rooms updated in the past 24
hours but at least one hour ago."
  :group 'leman-tabulated-room-list-faces)

(defface leman-tabulated-room-list-very-recent
  '((t (:inherit error)))
  "Latest timestamp of very recently updated rooms.
The foreground color is used to generate a gradient of colors
from recent to non-recent for rooms updated in the past hour."
  :group 'leman-tabulated-room-list-faces)

;;;; Bookmark support

;; Especially useful with Burly: <https://github.com/alphapapa/burly.el>

(require 'bookmark)

(defun leman-tabulated-room-list-bookmark-make-record ()
  "Return a bookmark record for the `leman-tabulated-room-list' buffer."
  (pcase-let* (((cl-struct leman-session user) leman-session)
               ((cl-struct leman-user (id session-id)) user))
    ;; MAYBE: Support bookmarking specific events in a room.
    (list (concat "Leman room list (" session-id ")")
          (cons 'session-id session-id)
          (cons 'handler #'leman-tabulated-room-list-bookmark-handler))))

(defun leman-tabulated-room-list-bookmark-handler (bookmark)
  "Show Leman room list buffer for BOOKMARK."
  (pcase-let* (((map session-id) bookmark))
    (unless (alist-get session-id leman-sessions nil nil #'equal)
      ;; MAYBE: Automatically connect.
      (user-error "Session %s not connected: call `leman-connect' first" session-id))
    (leman-tabulated-room-list)))

;;;; Commands

(defun leman-tabulated-room-list-next-unread ()
  "Show next unread room."
  (interactive)
  (unless (button-at (point))
    (call-interactively #'forward-button))
  (unless (cl-loop with starting-line = (line-number-at-pos)
                   if (equal "U" (elt (tabulated-list-get-entry) 0))
                   do (progn
                        (goto-char (button-end (button-at (point))))
                        (push-button (1- (point)))
                        (cl-return t))
                   else do (call-interactively #'forward-button)
                   while (> (line-number-at-pos) starting-line))
    ;; No more unread rooms.
    (message "No more unread rooms")))

;;;###autoload
(defun leman-tabulated-room-list (&rest _ignore)
  "Show buffer listing joined rooms.
Calls `pop-to-buffer-same-window'.  Interactively, with prefix,
call `pop-to-buffer'."
  (interactive)
  (with-current-buffer (get-buffer-create "*Leman Rooms*")
    (leman-tabulated-room-list-mode)
    (setq-local bookmark-make-record-function #'leman-tabulated-room-list-bookmark-make-record)
    ;; FIXME: There must be a better way to handle this.
    (funcall (if current-prefix-arg
                 #'pop-to-buffer #'pop-to-buffer-same-window)
             (current-buffer))))

(defun leman-tabulated-room-list--timestamp-colors ()
  "Return a vector of generated latest-timestamp colors for rooms.
Used in `leman-tabulated-room-list' and `leman-room-list'."
  (if (or (equal "unspecified-fg" (face-foreground 'default nil 'default))
          (equal "unspecified-bg" (face-background 'default nil 'default)))
      ;; NOTE: On a TTY, the default face's foreground and background colors may be the
      ;; special values "unspecified-fg"/"unspecified-bg", in which case we can't generate
      ;; gradients, so we just return a vector of "unspecified-fg".  See
      ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=55623>.
      (make-vector 134 "unspecified-fg")
    (cl-coerce
     (append (mapcar
              ;; One face per 10-minute period, from "recent" to 1-hour.
              (lambda (rgb)
                (pcase-let ((`(,r ,g ,b) rgb))
                  (color-rgb-to-hex r g b 2)))
              (color-gradient (color-name-to-rgb (face-foreground 'leman-tabulated-room-list-very-recent
                                                                  nil 'default))
                              (color-name-to-rgb (face-foreground 'leman-tabulated-room-list-recent
                                                                  nil 'default))
                              6))
             (mapcar
              ;; One face per hour, from "recent" to default.
              (lambda (rgb)
                (pcase-let ((`(,r ,g ,b) rgb))
                  (color-rgb-to-hex r g b 2)))
              (color-gradient (color-name-to-rgb (face-foreground 'leman-tabulated-room-list-recent
                                                                  nil 'default))
                              (color-name-to-rgb (face-foreground 'default nil 'default))
                              24))
             (mapcar
              ;; One face per week for the last year (actually we
              ;; generate colors for the past two years' worth so
              ;; that the face for one-year-ago is halfway to
              ;; invisible, and we don't use colors past that point).
              (lambda (rgb)
                (pcase-let ((`(,r ,g ,b) rgb))
                  (color-rgb-to-hex r g b 2)))
              (color-gradient (color-name-to-rgb (face-foreground 'default nil 'default))
                              (color-name-to-rgb (face-background 'default nil 'default))
                              104)))
     'vector)))

(define-derived-mode leman-tabulated-room-list-mode tabulated-list-mode
  "Leman-Tabulated-Room-List"
  :group 'leman
  (setf tabulated-list-format (vector
                               '("U" 1 t)
                               '(#("P" 0 1 (help-echo "Priority (favorite/low)")) 1 t)
                               '("B" 1 t)
                               ;; '("U" 1 t)
                               '("d" 1 t) ; Direct
                               (list (propertize "🐱"
                                                 'help-echo "Avatar")
                                     4 t) ; Avatar
                               '("Name" 25 t) '("Topic" 35 t)
                               (list "Latest"
                                     (if leman-tabulated-room-list-simplify-timestamps
                                         6 20)
                                     #'leman-tabulated-room-list-latest<
				     :right-align t)
                               '("Members" 7 leman-tabulated-room-list-members<)
                               ;; '("P" 1 t) '("Tags" 15 t)
                               '("Session" 15 t))
        tabulated-list-sort-key '("Latest" . t)
        leman-tabulated-room-list-timestamp-colors (leman-tabulated-room-list--timestamp-colors))
  (add-hook 'tabulated-list-revert-hook #'leman-tabulated-room-list--set-entries nil 'local)
  (tabulated-list-init-header)
  (leman-tabulated-room-list--set-entries)
  (tabulated-list-revert))

(defun leman-tabulated-room-list-action (event)
  "Show buffer for room at EVENT or point."
  (interactive "e")
  (mouse-set-point event)
  (pcase-let* ((room (tabulated-list-get-id))
               (`[,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name ,_topic ,_latest ,_members ,user-id]
                (tabulated-list-get-entry))
               (session (alist-get user-id leman-sessions nil nil #'equal)))
    (leman-view-room room session)))

;;;; Functions

;;;###autoload
(defun leman-tabulated-room-list-auto-update (_session)
  "Automatically update the room list buffer.
Does so when variable `leman-tabulated-room-list-auto-update' is non-nil.
To be called in `leman-sync-callback-hook'."
  (when (and leman-tabulated-room-list-auto-update
             (buffer-live-p (get-buffer "*Leman Rooms*")))
    (with-current-buffer (get-buffer "*Leman Rooms*")
      (revert-buffer))))

(defun leman-tabulated-room-list--set-entries ()
  "Set `tabulated-list-entries'."
  ;; Reset avatar size in case default font size has changed.
  ;; TODO: After implementing avatars.
  ;; (customize-set-variable 'leman-room-avatar-in-buffer-name-size leman-room-avatar-in-buffer-name-size)

  ;; NOTE: From Emacs docs:

  ;; This buffer-local variable specifies the entries displayed in the
  ;; Tabulated List buffer.  Its value should be either a list, or a
  ;; function.
  ;;
  ;; If the value is a list, each list element corresponds to one entry,
  ;; and should have the form ‘(ID CONTENTS)’, where
  ;;
  ;; • ID is either ‘nil’, or a Lisp object that identifies the
  ;; entry.  If the latter, the cursor stays on the same entry when
  ;; re-sorting entries.  Comparison is done with ‘equal’.
  ;;
  ;; • CONTENTS is a vector with the same number of elements as
  ;; ‘tabulated-list-format’.  Each vector element is either a
  ;;  string, which is inserted into the buffer as-is, or a list
  ;;  ‘(LABEL . PROPERTIES)’, which means to insert a text button by
  ;;   calling ‘insert-text-button’ with LABEL and PROPERTIES as
  ;;   arguments (*note Making Buttons::).
  ;;
  ;;   There should be no newlines in any of these strings.
  (let ((entries (cl-loop for (_id . session) in leman-sessions
                          append (mapcar (lambda (room)
                                           (leman-tabulated-room-list--entry session room))
                                         (leman-session-rooms session)))))
    (setf tabulated-list-entries
          ;; Pre-sort by latest event so that, when the list is sorted by other columns,
          ;; the rooms will be secondarily sorted by latest event.
          (cl-sort entries #'> :key (lambda (entry)
                                      ;; In case a room has no latest event (not sure if
                                      ;; this may obscure a bug, but this has happened, so
                                      ;; we need to handle it), we fall back to 0.
                                      (or (leman-room-latest-ts (car entry)) 0))))))

(defun leman-tabulated-room-list--avatar (room avatar room-list-avatar)
  "Return the avatar display string for ROOM's AVATAR.
ROOM-LIST-AVATAR is the cached, resized avatar string, if any."
  (if (and leman-tabulated-room-list-avatars avatar)
      (or room-list-avatar
          (if-let* ((avatar-image (get-text-property 0 'display avatar))
                    (new-avatar-string (propertize " " 'display
                                                   (leman--resize-image avatar-image
                                                                        nil (frame-char-height)))))
              (progn
                ;; alist-get doesn't seem to return the new value when used with setf?
                (setf (alist-get 'room-list-avatar (leman-room-local room))
                      new-avatar-string)
                new-avatar-string)
            ;; If a room avatar image fails to download or decode
            ;; and ends up nil, we return the empty string.
            (leman-debug "nil avatar for room: " (leman-room-display-name room) (leman-room-canonical-alias room))
            ""))
    ;; Room avatars disabled.
    ""))

(defun leman-tabulated-room-list--latest-face (latest-ts)
  "Return the face spec for a room's LATEST-TS timestamp.
Colors are taken from `leman-tabulated-room-list-timestamp-colors'."
  (when latest-ts
    (let* ((difference-seconds (- (float-time) (/ latest-ts 1000)))
           (n (cl-typecase difference-seconds
                ((number 0 3599) ;; 1 hour to 1 day: 24 1-hour periods.
                 (truncate (/ difference-seconds 600)))
                ((number 3600 86400) ;; 1 day
                 (+ 6 (truncate (/ difference-seconds 3600))))
                (otherwise ;; Difference in weeks.
                 (min (/ (length leman-tabulated-room-list-timestamp-colors) 2)
                      (+ 24 (truncate (/ difference-seconds 86400 7))))))))
      (list :foreground (elt leman-tabulated-room-list-timestamp-colors n)))))

(defun leman-tabulated-room-list--name-face (room buffer session status)
  "Return the face specification for ROOM's entry name.
BUFFER is the room's buffer, if any, SESSION the room's session,
and STATUS the room's status."
  ;; We have to copy the list, otherwise using `setf' on it
  ;; later causes its value to be mutated for every entry.
  (let ((face (cl-copy-list '(:inherit (leman-tabulated-room-list-name)))))
    ;; Add face modifiers.
    (when (and buffer (buffer-modified-p buffer))
      (push 'leman-tabulated-room-list-unread (map-elt face :inherit)))
    (when (leman--room-direct-p room session)
      (push 'leman-tabulated-room-list-direct (map-elt face :inherit)))
    (when (leman--room-favourite-p room)
      (push 'leman-tabulated-room-list-favourite (map-elt face :inherit)))
    (when (leman--room-low-priority-p room)
      (push 'leman-tabulated-room-list-low-priority (map-elt face :inherit)))
    (pcase status
      ('invite (push 'leman-tabulated-room-list-invited (map-elt face :inherit)))
      ('leave (push 'leman-tabulated-room-list-left (map-elt face :inherit))))
    face))

(defun leman-tabulated-room-list--entry (session room)
  "Return entry for ROOM in SESSION for `tabulated-list-entries'."
  (pcase-let* (((cl-struct leman-room id canonical-alias display-name avatar topic latest-ts summary
                           (local (map buffer room-list-avatar)))
                room)
               ((map ('m.joined_member_count member-count)) summary)
               (e-alias (or canonical-alias
                            (setf (leman-room-canonical-alias room)
                                  (leman--room-alias room))
                            id))
               ;; FIXME: Figure out how to track unread status cleanly.
               (e-unread (if (and buffer (buffer-modified-p buffer))
                             (propertize "U" 'help-echo "Unread") ""))
               (e-buffer (if buffer (propertize "B" 'help-echo "Room has buffer") ""))
               (e-avatar (leman-tabulated-room-list--avatar room avatar room-list-avatar))
               (name-face (leman-tabulated-room-list--name-face room buffer session (leman-room-status room)))
               (e-name (list (propertize (or display-name
                                             (leman--room-display-name room))
                                         ;; HACK: Apply face here, otherwise tabulated-list overrides it.
                                         'face name-face
                                         'help-echo e-alias)
                             'action #'leman-tabulated-room-list-action))
               (e-topic (if topic
                            ;; Remove newlines from topic.  Yes, this can happen.
                            (replace-regexp-in-string "\n" "" topic t t)
                          ""))
               (formatted-timestamp (if latest-ts
                                        (leman--human-format-duration (- (time-convert nil 'integer) (/ latest-ts 1000))
                                                                      t)
                                      ""))
               (latest-face (leman-tabulated-room-list--latest-face latest-ts))
               (e-latest (or (when formatted-timestamp
                               (propertize formatted-timestamp
                                           'value latest-ts
                                           'face latest-face))
                             ;; Invited rooms don't have a latest-ts.
                             ""))
               (e-session (propertize (leman-user-id (leman-session-user session))
                                      'value session))
               (e-direct-p (if (leman--room-direct-p room session)
                               (propertize "d" 'help-echo "Direct room")
                             ""))
               (e-priority (cond ((leman--room-favourite-p room) "F")
                                 ((leman--room-low-priority-p room) "l")
                                 (" ")))
               (e-members (if member-count (number-to-string member-count) "")))
    (when leman-tabulated-room-list-simplify-timestamps
      (setf e-latest (replace-regexp-in-string
                      (rx bos (1+ digit) (1+ alpha) (group (1+ (1+ digit) (1+ alpha))))
                      "" e-latest t t 1)))
    ;; Prefix the topic to show the room's status.
    (pcase (leman-room-status room)
      ('invite
       (setf e-topic (concat (propertize "[invited]"
                                         'face 'leman-tabulated-room-list-invited)
                             " " e-topic)))
      ('leave
       (setf e-topic (concat (propertize "[left]"
                                         'face 'leman-tabulated-room-list-left)
                             " " e-topic))))
    (list room (vector e-unread e-priority e-buffer e-direct-p
                       e-avatar e-name e-topic e-latest e-members
                       e-session))))
;; TODO: Define sorters with a macro?  This gets repetitive and hard to update.

(defun leman-tabulated-room-list-members< (a b)
  "Return non-nil if entry A has fewer members than room B.
A and B should be entries from `tabulated-list-mode'."
  (pcase-let* ((`(,_room [,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name-for-list ,_topic ,_latest ,a-members ,_session]) a)
               (`(,_room [,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name-for-list ,_topic ,_latest ,b-members ,_session]) b))
    (when (and a-members b-members)
      ;; Invited rooms may have no member count (I think).
      (< (string-to-number a-members) (string-to-number b-members)))))

(defun leman-tabulated-room-list-latest< (a b)
  "Return non-nil if entry A's latest event is older than entry B's.
A and B should be entries from `tabulated-list-mode'.  Rooms with
no latest event (e.g. invited rooms) sort first."
  (pcase-let* ((`(,_room-a [,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name-for-list ,_topic ,a-latest ,_a-members ,_session]) a)
               (`(,_room-b [,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name-for-list ,_topic ,b-latest ,_b-members ,_session]) b)
               (a-latest (get-text-property 0 'value a-latest))
               (b-latest (get-text-property 0 'value b-latest)))
    (cond ((and a-latest b-latest)
           (< a-latest b-latest))
          (b-latest
           ;; Invited rooms have no latest timestamp, and we want to sort them first.
           nil)
          (t t))))

;;;; Footer

(provide 'leman-tabulated-room-list)

;;; leman-tabulated-room-list.el ends here
