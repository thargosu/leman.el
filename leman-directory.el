;;; leman-directory.el --- Public room directory support                       -*- lexical-binding: t; -*-

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

;; This library provides support for viewing and searching public room directories on
;; Matrix homeservers.

;; To make rendering the list flexible and useful, we'll use `taxy-magit-section'.

;;; Code:

;;;; Requirements

(require 'leman)
(require 'leman-room-list)

(require 'taxy)
(require 'taxy-magit-section)

;;;; Variables

(defvar leman-directory-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'leman-directory-RET)
    (define-key map [mouse-1] #'leman-directory-mouse-1)
    (define-key map (kbd "+") #'leman-directory-next)
    map))

(defgroup leman-directory nil
  "Options for room directories."
  :group 'leman)

;;;; Mode

(define-derived-mode leman-directory-mode magit-section-mode "Leman-Directory"
  :global nil)

(defvar-local leman-directory-etc nil
  "Alist storing information in `leman-directory' buffers.")

;;;;; Keys

(eval-and-compile
  (taxy-define-key-definer leman-directory-define-key
    leman-directory-keys "leman-directory-key" "FIXME: Docstring."))

;; TODO: Other keys like guest_can_join, world_readable, etc.  (Last-updated time would be
;; nice, but the server doesn't include that in the results.)

(leman-directory-define-key joined-p ()
  (pcase-let (((map ('room_id id)) item)
              ((map session) leman-directory-etc))
    (when (cl-find id (leman-session-rooms session)
                   :key #'leman-room-id :test #'equal)
      "Joined")))

(leman-directory-define-key size (&key < >)
  (pcase-let (((map ('num_joined_members size)) item))
    (cond ((and < (< size <))
           (format "< %s members" <))
          ((and > (> size >))
           (format "> %s members" >)))))

(leman-directory-define-key space-p ()
  "Groups rooms that are themselves spaces."
  (pcase-let (((map ('room_type type)) item))
    (when (equal "m.space" type)
      "Spaces")))

(leman-directory-define-key people-p ()
  (pcase-let (((map ('room_id id) ('room_type type)) item)
              ((map session) leman-directory-etc))
    (pcase type
      ("m.space" nil)
      (_ (when-let ((room (cl-find id (leman-session-rooms session)
                                   :key #'leman-room-id :test #'equal))
                    ((leman--room-direct-p room session)))
           (leman-propertize "People"
             'face 'leman-room-list-direct))))))

(defcustom leman-directory-default-keys
  '((joined-p
     (people-p)
     (and :name "Rooms"
          :keys ((not people-p))))
    (space-p)
    ((size :> 10000))
    ((size :> 1000))
    ((size :> 100))
    ((size :> 10))
    ((size :< 11)))
  "Default keys."
  :type 'sexp)

;;;; Columns

(defvar-local leman-directory-room-avatar-cache (make-hash-table)
  ;; Use a buffer-local variable so that the cache is cleared when the buffer is closed.
  "Hash table caching room avatars for the `leman-directory' room list.")

(eval-and-compile
  (taxy-magit-section-define-column-definer "leman-directory"))

;; TODO: Fetch avatars (with queueing and async updating/insertion?).

(leman-directory-define-column #("✓" 0 1 (help-echo "Joined")) ()
  (pcase-let (((map ('room_id id)) item)
              ((map session) leman-directory-etc))
    (if (cl-find id (leman-session-rooms session)
                 :key #'leman-room-id :test #'equal)
        "✓"
      " ")))

(leman-directory-define-column "Name" (:max-width 25)
  (pcase-let* (((map name ('room_id id) ('room_type type)
                     ('canonical_alias canonical-alias))
                item)
               ((map session) leman-directory-etc)
               (room)
               (face (pcase type
                       ("m.space" 'leman-room-list-space)
                       (_ (if (and (setf room (cl-find id (leman-session-rooms session)
                                                       :key #'leman-room-id :test #'equal))
                                   (leman--room-direct-p room session))
                              'leman-room-list-direct
                            'leman-room-list-name)))))
    ;; NOTE: We can't use `leman--room-display-name' because these aren't room structs,
    ;; and we don't have membership data.
    (leman-propertize (or name canonical-alias "[unnamed]")
      'face face)))

(leman-directory-define-column "Alias" (:max-width 25)
  (pcase-let (((map ('canonical_alias alias)) item))
    (or alias "")))

(leman-directory-define-column "Size" (:align 'right)
  (pcase-let (((map ('num_joined_members size)) item))
    (number-to-string size)))

(leman-directory-define-column "Topic" (:max-width 50)
  (pcase-let (((map topic) item))
    (if topic
        (replace-regexp-in-string "\n" " | " topic nil t)
      "")))

(leman-directory-define-column "ID" ()
  (pcase-let (((map ('room_id id)) item))
    id))

(unless leman-directory-columns
  ;; TODO: Automate this or document it
  (setq-default leman-directory-columns
                '("Name" "Alias" "Size" "Topic" "ID")))

;;;; Commands

;; TODO: Pagination of results.

;;;###autoload
(cl-defun leman-directory (&key server session since (limit 100))
  "View the public room directory on SERVER with SESSION.
Show up to LIMIT rooms.  Interactively, with prefix, prompt for
server and LIMIT.

SINCE may be a next-batch token."
  (interactive (let* ((session (leman-complete-session :prompt "Search on session: "))
                      (server (if current-prefix-arg
                                  (read-string "Search on server: " nil nil
                                               (leman-server-name (leman-session-server session)))
                                (leman-server-name (leman-session-server session))))
                      (args (list :server server :session session)))
                 (when current-prefix-arg
                   (cl-callf plist-put args
                     :limit (read-number "Limit number of rooms: " 100)))
                 args))
  (pcase-let ((revert-function (lambda (&rest _ignore)
                                 (interactive)
                                 (leman-directory :server server :session session :limit limit)))
              (endpoint "publicRooms")
              (params (list (list "limit" limit))))
    (when since
      (cl-callf append params (list (list "since" since))))
    (leman-api session endpoint :params params
      :then (lambda (results)
              (pcase-let (((map ('chunk rooms) ('next_batch next-batch)
                                ('total_room_count_estimate remaining))
                           results))
                (leman-directory--view rooms :append-p since
                  :buffer-name (format "*Leman Directory: %s*" server)
                  :root-section-name (format "Leman Directory: %s" server)
                  :init-fn (lambda ()
                             (setf (alist-get 'server leman-directory-etc) server
                                   (alist-get 'session leman-directory-etc) session
                                   (alist-get 'next-batch leman-directory-etc) next-batch
                                   (alist-get 'limit leman-directory-etc) limit)
                             (setq-local revert-buffer-function revert-function)
                             (when remaining
                               ;; FIXME: The server seems to report all of the rooms on
                               ;; the server as remaining even when searching for a
                               ;; specific term like "emacs".
                               ;; TODO: Display this in a more permanent place (like a
                               ;; header or footer).
                               (message
                                (substitute-command-keys
                                 "%s rooms remaining (use \\[leman-directory-next] to fetch more)")
                                remaining)))))))
    (leman-message "Listing %s rooms on %s..." limit server)))

;;;###autoload
(cl-defun leman-directory-search (query &key server session since (limit 1000))
  "View public rooms on SERVER matching QUERY.
QUERY is a string used to filter results."
  (interactive (let* ((session (leman-complete-session :prompt "Search on session: "))
                      (server (if current-prefix-arg
                                  (read-string "Search on server: " nil nil
                                               (leman-server-name (leman-session-server session)))
                                (leman-server-name (leman-session-server session))))
                      (query (read-string (format "Search for rooms on %s matching: " server)))
                      (args (list query :server server :session session)))
                 (when current-prefix-arg
                   (cl-callf plist-put (cdr args)
                     :limit (read-number "Limit number of rooms: " 1000)))
                 args))
  ;; TODO: Handle "include_all_networks" and "third_party_instance_id".  See § 10.5.4.
  (pcase-let* ((revert-function (lambda (&rest _ignore)
                                  (interactive)
                                  (leman-directory-search query :server server :session session)))
               (endpoint "publicRooms")
               (data (rassq-delete-all nil
                                       (leman-alist "limit" limit
                                                    "filter" (leman-alist "generic_search_term" query)
                                                    "since" since))))
    (leman-api session endpoint :method 'post :data (json-encode data)
      :then (lambda (results)
              (pcase-let (((map ('chunk rooms) ('next_batch next-batch)
                                ('total_room_count_estimate remaining))
                           results))
                (leman-directory--view rooms :append-p since
                  :buffer-name (format "*Leman Directory: \"%s\" on %s*" query server)
                  :root-section-name (format "Leman Directory: \"%s\" on %s" query server)
                  :init-fn (lambda ()
                             (setf (alist-get 'server leman-directory-etc) server
                                   (alist-get 'session leman-directory-etc) session
                                   (alist-get 'next-batch leman-directory-etc) next-batch
                                   (alist-get 'limit leman-directory-etc) limit
                                   (alist-get 'query leman-directory-etc) query)
                             (setq-local revert-buffer-function revert-function)
                             (when remaining
                               (message
                                (substitute-command-keys
                                 "%s rooms remaining (use \\[leman-directory-next] to fetch more)")
                                remaining)))))))
    (leman-message "Searching for %S on %s..." query server)))

(defun leman-directory-next ()
  "Fetch next batch of results in `leman-directory' buffer."
  (interactive)
  (pcase-let (((map next-batch query limit server session) leman-directory-etc))
    (unless next-batch
      (user-error "No more results"))
    (if query
        (leman-directory-search query :server server :session session :limit limit :since next-batch)
      (leman-directory :server server :session session :limit limit :since next-batch))))

(defun leman-directory-mouse-1 (event)
  "Call `leman-directory-RET' at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'leman-directory-RET))

(defun leman-directory-RET ()
  "View or join room at point, or cycle section at point."
  (interactive)
  (cl-etypecase (oref (magit-current-section) value)
    (null nil)
    (list (pcase-let* (((map ('name name) ('room_id room-id)) (oref (magit-current-section) value))
                       ((map session) leman-directory-etc)
                       (room (cl-find room-id (leman-session-rooms session)
                                      :key #'leman-room-id :test #'equal)))
            (if room
                (leman-view-room room session)
              ;; Room not joined: prompt to join.  (Don't use the alias in the prompt,
              ;; because multiple rooms might have the same alias, e.g. when one is
              ;; upgraded or tombstoned.)
              (when (yes-or-no-p (format "Join room \"%s\" <%s>? " name room-id))
                (leman-join-room room-id session)))))
    (taxy-magit-section (call-interactively #'magit-section-cycle))))

;;;; Functions

(cl-defun leman-directory--view (rooms &key init-fn append-p
                                       (buffer-name "*Leman Directory*")
                                       (root-section-name "Leman Directory")
                                       (keys leman-directory-default-keys)
                                       (display-buffer-action '(display-buffer-same-window)))
  "View ROOMS in an `leman-directory-mode' buffer.
ROOMS should be a list of rooms from an API request.  Calls
INIT-FN immediately after activating major mode.  Sets
BUFFER-NAME and ROOT-SECTION-NAME, and uses
DISPLAY-BUFFER-ACTION.  KEYS are a list of `taxy' keys.  If
APPEND-P, add ROOMS to buffer rather than replacing existing
contents.  To be called by `leman-directory-search'."
  (declare (indent defun))
  (let (column-sizes window-start)
    (cl-labels ((format-item (item)
                  ;; NOTE: We use the buffer-local variable `leman-directory-etc' rather
                  ;; than a closure variable because the taxy-magit-section struct's format
                  ;; table is not stored in it, and we can't reuse closures' variables.
                  ;; (It would be good to store the format table in the taxy-magit-section
                  ;; in the future, to make this cleaner.)
                  (gethash item (alist-get 'format-table leman-directory-etc)))
                ;; NOTE: Since these functions take an "item" (which is a [room session]
                ;; vector), they're prefixed "item-" rather than "room-".
                (size (item)
                  (pcase-let (((map ('num_joined_members size)) item))
                    size))
                (t<nil (a b) (and a (not b)))
                (t>nil (a b) (and (not a) b))
                (make-fn (&rest args)
                  (apply #'make-taxy-magit-section
                         :make #'make-fn
                         :format-fn #'format-item
                         ;; FIXME: Should we reuse `leman-room-list-level-indent' here?
                         :level-indent leman-room-list-level-indent
                         ;; :visibility-fn #'visible-p
                         ;; :heading-indent 2
                         :item-indent 2
                         ;; :heading-face-fn #'heading-face
                         args)))
      (with-current-buffer (get-buffer-create buffer-name)
        (unless (eq 'leman-directory-mode major-mode)
          ;; Don't obliterate buffer-local variables.
          (leman-directory-mode))
        (when init-fn
          (funcall init-fn))
        (pcase-let* ((taxy (if append-p
                               (alist-get 'taxy leman-directory-etc)
                             (make-fn
                              :name root-section-name
                              :take (taxy-make-take-function keys leman-directory-keys))))
                     (taxy-magit-section-insert-indent-items nil)
                     (inhibit-read-only t)
                     (pos (point))
                     (section-ident (when (magit-current-section)
                                      (magit-section-ident (magit-current-section))))
                     (format-cons))
          (setf taxy (thread-last taxy
                                  (taxy-fill (cl-coerce rooms 'list))
                                  (taxy-sort #'> #'size)
                                  (taxy-sort* #'string> #'taxy-name))
                (alist-get 'taxy leman-directory-etc) taxy
                format-cons (taxy-magit-section-format-items
                             leman-directory-columns leman-directory-column-formatters taxy)
                (alist-get 'format-table leman-directory-etc) (car format-cons)
                column-sizes (cdr format-cons)
                header-line-format (taxy-magit-section-format-header
                                    column-sizes leman-directory-column-formatters)
                window-start (if (get-buffer-window buffer-name)
                                 (window-start (get-buffer-window buffer-name))
                               0))
          (delete-all-overlays)
          (erase-buffer)
          (save-excursion
            (taxy-magit-section-insert taxy :items 'first
              ;; :blank-between-depth bufler-taxy-blank-between-depth
              :initial-depth 0))
          (goto-char pos)
          (when (and section-ident (magit-get-section section-ident))
            (goto-char (oref (magit-get-section section-ident) start)))))
      (display-buffer buffer-name display-buffer-action)
      (when (get-buffer-window buffer-name)
        (set-window-start (get-buffer-window buffer-name) window-start))
      ;; NOTE: In order for `bookmark--jump-via' to work properly, the restored buffer
      ;; must be set as the current buffer, so we have to do this explicitly here.
      (set-buffer buffer-name))))

;;;; Spaces

;; Viewing spaces and the rooms in them.

;;;###autoload
(defun leman-view-space (space session)
  ;; TODO: Use this for spaces instead of `leman-view-room' (or something like that).
  ;; TODO: Display space's topic in the header or something.
  "View child rooms in SPACE on SESSION.
SPACE may be a room ID or an `leman-room' struct."
  ;; TODO: "from" query parameter.
  (interactive (leman-complete-room :predicate #'leman--space-p
                 :prompt "Space: "))
  (pcase-let* ((id (cl-typecase space
                     (string space)
                     (leman-room (leman-room-id space))))
               (endpoint (format "rooms/%s/hierarchy" id))
               (revert-function (lambda (&rest _ignore)
                                  (interactive)
                                  (leman-view-space space session))))
    (leman-api session endpoint :version "v1"
      :then (lambda (results)
              (pcase-let (((map rooms ('next_batch next-batch))
                           results))
                (leman-directory--view rooms ;; :append-p since
                  ;; TODO: Use space's alias where possible.
                  :buffer-name (format "*Leman Directory: space %s" (leman--format-room space session))
                  :root-section-name (format "*Leman Directory: rooms in %s %s"
                                             (leman-propertize "space"
                                               'face 'font-lock-type-face)
                                             (leman--format-room space session))
                  :init-fn (lambda ()
                             (setf (alist-get 'session leman-directory-etc) session
                                   (alist-get 'next-batch leman-directory-etc) next-batch
                                   ;; (alist-get 'limit leman-directory-etc) limit
                                   (alist-get 'space leman-directory-etc) space)
                             (setq-local revert-buffer-function revert-function)
                             ;; TODO: Handle next batches.
                             ;; (when remaining
                             ;;   (message
                             ;;    (substitute-command-keys
                             ;;     "%s rooms remaining (use \\[leman-directory-next] to fetch more)")
                             ;;    remaining))
                             )))))))

;;;; Footer

(provide 'leman-directory)
;;; leman-directory.el ends here
