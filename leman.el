;;; leman.el --- Matrix client                       -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/thargosu/ement.el
;; Version: 0.18-pre
;; Package-Requires: ((emacs "27.1") (map "2.1") (persist "0.5") (plz "0.6") (taxy "0.10") (taxy-magit-section "0.13") (svg-lib "0.2.5") (transient "0.3.7"))
;; Keywords: comm

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

;; Another Matrix client!  This one is written from scratch and is
;; intended to be more "Emacsy," more suitable for MELPA, etc.  Also
;; it has a shorter, perhaps catchier name, that is a mildly clever
;; play on the name of the official Matrix client and the Emacs Lisp
;; filename extension (oops, I explained the joke), which makes for
;; much shorter symbol names.

;; This file implements the core client library.  Functions that may be called in multiple
;; files belong in `leman-lib'.

;;; Code:

;;;; Debugging

;; NOTE: Uncomment this form and `emacs-lisp-byte-compile-and-load' the file to enable
;; `leman-debug' messages.  This is commented out by default because, even though the
;; messages are only displayed when `warning-minimum-log-level' is `:debug' at runtime, if
;; that is so at expansion time, the expanded macro calls format the message and check the
;; log level at runtime, which is not zero-cost.

;; (eval-and-compile
;;   (require 'warnings)
;;   (setq-local warning-minimum-log-level nil)
;;   (setq-local warning-minimum-log-level :debug))

;;;; Requirements

;; Built in.
(require 'cl-lib)
(require 'dns)
(require 'files)
(require 'map)

;; This package.
(require 'leman-lib)
(require 'leman-room)
(require 'leman-notifications)
(require 'leman-notify)

;;;; Variables

(defvar leman-sessions nil
  "Alist of active `leman-session' sessions, keyed by MXID.")

(defvar leman-syncs nil
  "Alist of outstanding sync processes for each session.")

(defvar leman-users (make-hash-table :test #'equal)
  ;; NOTE: When changing the leman-user struct, it's necessary to
  ;; reset this table to clear old-type structs.
  "Hash table storing user structs keyed on user ID.")

(defvar leman-progress-reporter nil
  "Used to report progress while processing sync events.")

(defvar leman-progress-value nil
  "Used to report progress while processing sync events.")

(defvar leman-sync-callback-hook
  '(leman--update-room-buffers leman--auto-sync leman-tabulated-room-list-auto-update
                               leman-room-list-auto-update)
  "Hook run after `leman--sync-callback'.
Hooks are called with one argument, the session that was
synced.")

(defvar leman-event-hook
  '(leman-notify leman--process-event leman--put-event)
  "Hook called for events.
Each function is called with three arguments: the event, the
room, and the session.  This hook isn't intended to be modified
by users; ones who do so should know what they're doing.")

(defvar leman-default-sync-filter
  '((room (state (lazy_load_members . t))
          (timeline (lazy_load_members . t))))
  "Default filter for sync requests.")

(defvar leman-images-queue (make-plz-queue :limit 5)
  "`plz' HTTP request queue for image requests.")

(defvar leman-read-receipt-idle-timer nil
  "Idle timer used to update read receipts.")

(defvar leman-connect-user-id-history nil
  "History list of user IDs entered into `leman-connect'.")

;; From other files.
(defvar leman-room-avatar-max-width)
(defvar leman-room-avatar-max-height)

;;;; Customization

(defgroup leman-faces nil
  "Faces for Leman."
  :group 'leman)

(defgroup leman nil
  "Options for Leman, the Matrix client."
  :group 'comm)

(defcustom leman-save-sessions nil
  "Save session to disk.
Writes the session file when Emacs is killed."
  :type 'boolean
  :set (lambda (option value)
         (set-default option value)
         (if value
             (add-hook 'kill-emacs-hook #'leman--kill-emacs-hook)
           (remove-hook 'kill-emacs-hook #'leman--kill-emacs-hook))))

(defcustom leman-sessions-file "~/.cache/leman.el"
  ;; FIXME: Expand correct XDG cache directory (new in Emacs 27).
  "Save username and access token to this file."
  :type 'file)

(defcustom leman-auto-sync t
  "Automatically sync again after syncing."
  :type 'boolean)

(defcustom leman-after-initial-sync-hook
  '(leman-room-list--after-initial-sync leman-view-initial-rooms leman--link-children leman--run-idle-timer)
  "Hook run after initial sync.
Run with one argument, the session synced."
  :type 'hook)

(defcustom leman-initial-sync-timeout 40
  "Timeout in seconds for initial sync requests.
For accounts in many rooms, the Matrix server may take some time
to prepare the initial sync response, and increasing this timeout
might be necessary."
  :type 'integer)

(defcustom leman-auto-view-rooms nil
  "Rooms to view after initial sync.
Alist mapping user IDs to a list of room aliases/IDs to open buffers for."
  :type '(alist :key-type (string :tag "Local user ID")
                :value-type (repeat (string :tag "Room alias/ID"))))

(defcustom leman-disconnect-hook '(leman-kill-buffers leman--stop-idle-timer)
  ;; FIXME: Put private functions in a private hook.
  "Functions called when disconnecting.
That is, when calling command `leman-disconnect'.  Functions are
called with no arguments."
  :type 'hook)

(defcustom leman-view-room-display-buffer-action '(display-buffer-same-window)
  "Display buffer action to use when opening room buffers.
See function `display-buffer' and info node `(elisp) Buffer
Display Action Functions'."
  :type 'function)

(defcustom leman-auto-view-room-display-buffer-action '(display-buffer-no-window)
  "Display buffer action to use when automatically opening room buffers.
That is, rooms listed in `leman-auto-view-rooms', which see.  See
function `display-buffer' and info node `(elisp) Buffer Display
Action Functions'."
  :type 'function)

(defcustom leman-interrupted-sync-hook '(leman-interrupted-sync-warning)
  "Functions to call when syncing of a session is interrupted.
Only called when `leman-auto-sync' is non-nil.  Functions are
called with one argument, the session whose sync was interrupted.

This hook allows the user to customize how sync interruptions are
handled (e.g. how to be notified)."
  :type 'hook
  :options '(leman-interrupted-sync-message leman-interrupted-sync-warning))

(defcustom leman-sso-server-port 4567
  "TCP port used for local HTTP server for SSO logins.
It shouldn't usually be necessary to change this."
  :type 'integer)

;;;; Commands

(defun leman--new-session (user-id &optional uri-prefix)
  "Return a new session for USER-ID, using URI-PREFIX if given."
  (unless (string-match (rx bos "@" (group (1+ (not (any ":")))) ; Username
                            ":" (group (optional (1+ (not (any blank)))))) ; Server name
                        user-id)
    (user-error "Invalid user ID format: use @USERNAME:SERVER"))
  (let* ((username (match-string 1 user-id))
         (server-name (match-string 2 user-id))
         (uri-prefix (or uri-prefix (leman--hostname-uri server-name)))
         (user (make-leman-user :id user-id :username username))
         (server (make-leman-server :name server-name :uri-prefix uri-prefix))
         (transaction-id (leman--initial-transaction-id))
         (initial-device-display-name (format "Leman.el: %s@%s"
                                              ;; Just to be extra careful:
                                              (or user-login-name "[unknown user-login-name]")
                                              (or (system-name) "[unknown system-name]")))
         (device-id (secure-hash 'sha256 initial-device-display-name)))
    (make-leman-session :user user :server server :transaction-id transaction-id
                        :device-id device-id :initial-device-display-name initial-device-display-name
                        :events (make-hash-table :test #'equal))))

(defun leman--password-login (session &optional password)
  "Log in to SESSION using PASSWORD, prompting if not given."
  (pcase-let* (((cl-struct leman-session user device-id initial-device-display-name) session)
               ((cl-struct leman-user id) user)
               (data (leman-alist "type" "m.login.password"
                                  "identifier"
                                  (leman-alist "type" "m.id.user"
                                               "user" id)
                                  "password" (or password
                                                 (read-passwd (format "Password for %s: " id)))
                                  "device_id" device-id
                                  "initial_device_display_name" initial-device-display-name)))
    ;; TODO: Clear password in callback (if we decide to hold on to it for retrying login timeouts).
    (leman-api session "login" :method 'post :data (json-encode data)
      :then (apply-partially #'leman--login-callback session))
    (leman-message "Logging in with password...")))

(defun leman--sso-login-with-token (token session)
  "Submit SSO login TOKEN for SESSION."
  (pcase-let* (((cl-struct leman-session user device-id initial-device-display-name) session)
               ((cl-struct leman-user id) user)
               (data (leman-alist
                      "type" "m.login.token"
                      "identifier" (leman-alist "type" "m.id.user"
                                                "user" id)
                      "token" token
                      "device_id" device-id
                      "initial_device_display_name" initial-device-display-name)))
    (leman-api session "login" :method 'post
      :data (json-encode data)
      :then (apply-partially #'leman--login-callback session))))

(defun leman--sso-login (session)
  "Log in to SESSION using single sign-on.
Starts a throwaway, local HTTP server on `leman-sso-server-port'
to receive the login token, and browses to the server's SSO
redirect page."
  (let (sso-server-process)
    (setf sso-server-process
          (make-network-process
           :name "leman-sso" :family 'ipv4 :host 'local :service leman-sso-server-port
           :filter (lambda (process string)
                     ;; NOTE: This is technically wrong, because it's not guaranteed that the
                     ;; string will be a complete request--it could just be a chunk.  But in
                     ;; practice, if this works, it's much simpler than setting up process log
                     ;; functions and per-client buffers for this throwaway, pretend HTTP server.
                     (when (string-match (rx "GET /?loginToken=" (group (0+ nonl)) " " (0+ nonl)) string)
                       (unwind-protect
                           (progn
                             (leman--sso-login-with-token (match-string 1 string) session)
                             (process-send-string process "HTTP/1.0 202 Accepted
Content-Type: text/plain; charset=utf-8

Leman: SSO login accepted; session token received.  Connecting to Matrix server.  (You may close this page.)")
                             (process-send-eof process))
                         (delete-process sso-server-process)
                         (delete-process process))))
           :server t :noquery t))
    ;; Kill server after 2 minutes in case of problems.
    (run-at-time 120 nil (lambda ()
                           (when (process-live-p sso-server-process)
                             (delete-process sso-server-process))))
    (let ((url (concat (leman-server-uri-prefix (leman-session-server session))
                       "/_matrix/client/r0/login/sso/redirect?redirectUrl=http://localhost:"
                       (number-to-string leman-sso-server-port))))
      (funcall browse-url-secondary-browser-function url)
      (message "Browsing to single sign-on page <%s>..." url))))

(defun leman--login-with-flow (flow session &optional password)
  "Begin login FLOW (\"password\" or \"sso\") for SESSION.
PASSWORD, if given, is used for password login."
  (pcase flow
    ("password" (leman--password-login session password))
    ("sso" (leman--sso-login session))
    (_ (error "Leman: Unsupported login flow: %s  Server:%S"
              flow (leman-server-uri-prefix (leman-session-server session))))))

(defun leman--connect-flows-callback (session password data)
  "Begin a login flow supported by the server for SESSION.
PASSWORD, if given, is used for password login; otherwise the
user is prompted."
  (let ((flows (cl-loop for flow across (map-elt data 'flows)
                        for type = (map-elt flow 'type)
                        when (member type '("m.login.password" "m.login.sso"))
                        collect type)))
    (pcase (length flows)
      (0 (error "Leman: No supported login flows:  Server:%S  Supported flows:%S"
                (leman-server-uri-prefix (leman-session-server session))
                (map-elt data 'flows)))
      (1 (leman--login-with-flow (string-trim-left (car flows) (rx "m.login."))
                                 session password))
      (_ (leman--login-with-flow
          (completing-read "Select authentication method: "
                           (cl-loop for flow in flows
                                    collect (string-trim-left flow (rx "m.login."))))
          session password)))))

(defun leman--session-start-sync (session)
  "Register SESSION in `leman-sessions' and start syncing it."
  ;; HACK: If session is already in leman-sessions, this replaces it.  I think that's okay...
  (setf (alist-get (leman-user-id (leman-session-user session))
                   leman-sessions nil nil #'equal)
        session)
  (leman--sync session :timeout leman-initial-sync-timeout))

(defun leman--connect-args ()
  "Return arguments for interactively calling `leman-connect'.
With prefix arg, ignore any saved session and prompt to log in
again; otherwise, use a saved session if one is available."
  (if current-prefix-arg
      ;; Force new session.
      (list :user-id (read-string "User ID: " nil 'leman-connect-user-id-history))
    ;; Use known session.
    (unless leman-sessions
      ;; Read sessions from disk.
      (condition-case err
          (setf leman-sessions (leman--read-sessions))
        (error (display-warning 'leman (format "Unable to read session data from disk (%s).  Prompting to log in again."
                                               (error-message-string err))))))
    (cl-case (length leman-sessions)
      (0 (list :user-id (read-string "User ID: " nil 'leman-connect-user-id-history)))
      (1 (list :session (cdar leman-sessions)))
      (otherwise (list :session (leman-complete-session))))))

;;;###autoload
(cl-defun leman-connect (&key user-id password uri-prefix session)
  "Connect to Matrix with USER-ID and PASSWORD, or using SESSION.
Interactively, with prefix, ignore a saved session and log in
again; otherwise, use a saved session if `leman-save-sessions' is
enabled and a saved session is available, or prompt to log in if
not enabled or available.

If USER-ID or PASSWORD are not specified, the user will be
prompted for them.

If URI-PREFIX is specified, it should be the prefix of the
server's API URI, including protocol, hostname, and optionally
the port, e.g.

  \"https://matrix-client.matrix.org\"
  \"http://localhost:8080\""
  (interactive (leman--connect-args))
  (if session
      ;; Start syncing given session.
      (leman--session-start-sync session)
    ;; Start the login flow.  Prompt for user ID if not given (i.e. if
    ;; not called interactively).
    (unless user-id
      (setf user-id (read-string "User ID: " nil 'leman-connect-user-id-history)))
    (setf session (leman--new-session user-id uri-prefix))
    (when (leman-api session "login"
            :then (apply-partially #'leman--connect-flows-callback session password))
      (message "Leman: Checking server's login flows..."))))
(defun leman-disconnect (sessions)
  "Disconnect from SESSIONS.
Interactively, with prefix, disconnect from all sessions.  If
`leman-auto-sync' is enabled, stop syncing, and clear the session
data.  When enabled, write the session to disk.  Any existing
room buffers are left alive and can be read, but other commands
in them won't work."
  (interactive (list (if current-prefix-arg
                         (mapcar #'cdr leman-sessions)
                       (list (leman-complete-session)))))
  (when leman-save-sessions
    ;; Write sessions before we remove them from the variable.
    (leman--write-sessions leman-sessions))
  (dolist (session sessions)
    (let ((user-id (leman-user-id (leman-session-user session))))
      (when-let ((process (map-elt leman-syncs session)))
        ;; Disable the sync process's ELSE handler, preventing error messages, but still
        ;; allowing `plz--respond' to clean up the buffer, etc.
        (setf (process-get process :plz-else) #'ignore)
        (delete-process process))
      ;; NOTE: I'd like to use `map-elt' here, but not until
      ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47368> is fixed, I guess.
      (setf (alist-get session leman-syncs nil nil #'equal) nil
            (alist-get user-id leman-sessions nil 'remove #'equal) nil)))
  (unless leman-sessions
    ;; HACK: If no sessions remain, clear the users table.  It might be best
    ;; to store a per-session users table, but this is probably good enough.
    (clrhash leman-users))
  (run-hooks 'leman-disconnect-hook)
  (message "Leman: Disconnected <%s>."
           (string-join (cl-loop for session in sessions
                                 collect (leman-user-id (leman-session-user session)))
                        ", ")))

(defun leman-kill-buffers ()
  "Kill all Leman buffers.
Useful in, e.g. `leman-disconnect-hook', which see."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "leman-" (symbol-name (buffer-local-value 'major-mode buffer)))
      (kill-buffer buffer))))

(defun leman--login-callback (session data)
  "Record DATA from logging in to SESSION and do initial sync."
  (pcase-let* (((map ('access_token token) ('device_id device-id)) data))
    (setf (leman-session-token session) token
          (leman-session-device-id session) device-id)
    (leman--session-start-sync session)))

;;;; Functions

(defun leman-interrupted-sync-warning (session)
  "Display a warning that syncing of SESSION was interrupted."
  (display-warning
   'leman
   (format
    (substitute-command-keys
     "\\<leman-room-mode-map>Syncing of session <%s> was interrupted.  Use command `leman-room-sync' in a room buffer to retry.")
    (leman-user-id (leman-session-user session)))
   :error))

(defun leman-interrupted-sync-message (session)
  "Display a message that syncing of SESSION was interrupted."
  (message
   (substitute-command-keys
    "\\<leman-room-mode-map>Syncing of session <%s> was interrupted.  Use command `leman-room-sync' in a room buffer to retry.")
   (leman-user-id (leman-session-user session))))

(defun leman--run-idle-timer (&rest _ignore)
  "Run idle timer that updates read receipts.
To be called from `leman-after-initial-sync-hook'.  Timer is
stored in `leman-read-receipt-idle-timer'."
  (unless (timerp leman-read-receipt-idle-timer)
    (setf leman-read-receipt-idle-timer (run-with-idle-timer 3 t #'leman-room-read-receipt-idle-timer))))

(defun leman--stop-idle-timer (&rest _ignore)
  "Stop idle timer stored in `leman-read-receipt-idle-timer'.
To be called from `leman-disconnect-hook'."
  (unless leman-sessions
    (when (timerp leman-read-receipt-idle-timer)
      (cancel-timer leman-read-receipt-idle-timer)
      (setf leman-read-receipt-idle-timer nil))))

(defun leman-view-initial-rooms (session)
  "View rooms for SESSION configured in `leman-auto-view-rooms'."
  (when-let (rooms (alist-get (leman-user-id (leman-session-user session))
			      leman-auto-view-rooms nil nil #'equal))
    (dolist (alias/id rooms)
      (when-let (room (cl-find-if (lambda (room)
				    (or (equal alias/id (leman-room-canonical-alias room))
					(equal alias/id (leman-room-id room))))
				  (leman-session-rooms session)))
        (let ((leman-view-room-display-buffer-action leman-auto-view-room-display-buffer-action))
          (leman-view-room room session))))))

(defun leman--initial-transaction-id ()
  "Return an initial transaction ID for a new session."
  ;; We generate a somewhat-random initial transaction ID to avoid potential conflicts in
  ;; case, e.g. using Pantalaimon causes a transaction ID conflict.  See
  ;; <https://github.com/alphapapa/ement.el/issues/36>.
  (cl-parse-integer
   (secure-hash 'sha256 (prin1-to-string (list (current-time) (system-name))))
   :end 8 :radix 16))

(defsubst leman--sync-messages-p (session)
  "Return non-nil if sync-related messages should be shown for SESSION."
  ;; For now, this seems like the best way.
  (or (not (leman-session-has-synced-p session))
      (not leman-auto-sync)))

(defun leman--hostname-uri (hostname)
  "Return the \".well-known\" URI for server HOSTNAME.
If no URI is found, prompt the user for the hostname."
  ;; FIXME: When fail-prompting, a URI should be returned, not just a hostname.
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id178> ("4.1   Well-known URI")
  (cl-labels ((fail-prompt ()
                (let ((input (read-string "Auto-discovery of server's well-known URI failed.  Input server hostname, or leave blank to use server name: ")))
                  (pcase input
                    ("" hostname)
                    (_ input))))
              (parse (string)
                (if-let* ((object (ignore-errors (json-read-from-string string)))
                          (url (map-nested-elt object '(m.homeserver base_url)))
                          ((string-match-p
                            (rx bos "http" (optional "s") "://" (1+ nonl))
                            url)))
                    url
                  ;; Parsing error: FAIL_PROMPT.
                  (fail-prompt))))
    (condition-case err
        (let ((response (plz 'get (concat "https://" hostname "/.well-known/matrix/client")
                          :as 'response :then 'sync)))
          (if (plz-response-p response)
              (pcase (plz-response-status response)
                (200 (parse (plz-response-body response)))
                (404 (fail-prompt))
                (_ (warn "Leman: `plz' request for .well-known URI returned unexpected code: %s"
                         (plz-response-status response))
                   (fail-prompt)))
            (warn "Leman: `plz' request for .well-known URI did not return a `plz' response")
            (fail-prompt)))
      (error (warn "Leman: `plz' request for .well-known URI signaled an error: %S" err)
             (fail-prompt)))))

(defun leman--sync-maybe-interrupt (session force)
  "Interrupt any outstanding sync for SESSION.
If FORCE is nil, signal an error instead."
  (when (map-elt leman-syncs session)
    (if force
        (condition-case err
            (delete-process (map-elt leman-syncs session))
          ;; Ensure the only error is the expected one from deleting the process.
          (leman-api-error (cl-assert (equal "curl process killed" (plz-error-message (cl-third err))))
                           (message "Leman: Forcing new sync")))
      (user-error "Leman: Already syncing this session"))))

(defun leman--sync-params (next-batch filter)
  "Return query parameters for a sync request for NEXT-BATCH and FILTER."
  ;; TODO: Document filter arg.
  (remove
   nil (list (list "full_state" (if next-batch "false" "true"))
             (when filter
               (list "filter" (json-encode filter)))
             (when next-batch
               (list "since" next-batch))
             (when next-batch
               (list "timeout" "30000")))))

(defun leman--sync-failed (session timeout plz-error)
  "Handle a failed sync request for SESSION.
TIMEOUT is the request's timeout, which is used when re-syncing.
PLZ-ERROR is the error passed by `plz'."
  (setf (map-elt leman-syncs session) nil)
  ;; TODO: plz probably needs nicer error handling.
  ;; Ideally we would use `condition-case', but since the error is
  ;; signaled in `plz--sentinel'...
  (pcase-let* (((cl-struct plz-error curl-error response) plz-error)
               (reason))
    (cond ((when response
             (pcase (plz-response-status response)
               ((or 429 502) (setf reason "failed")))))
          ((pcase curl-error
             (`(28 . ,_) (setf reason "timed out")))))
    (if reason
        (if (not leman-auto-sync)
            (run-hook-with-args 'leman-interrupted-sync-hook session)
          (message "Leman: Sync %s (%s).  Syncing again..."
                   reason (leman-user-id (leman-session-user session)))
          ;; Set QUIET to allow the just-printed message to remain visible.
          (leman--sync session :timeout timeout :quiet t))
      ;; Unrecognized errors:
      (pcase curl-error
        (`(,code . ,message)
         (signal 'leman-api-error (list (format "Leman: Network error: %s: %s" code message)
                                        plz-error)))
        (_ (signal 'leman-api-error (list "Leman: Unrecognized network error" plz-error)))))))

(defun leman--sync-read-json (session sync-start-time)
  "Print a message, then parse the sync response for SESSION.
Called in the buffer holding the response; SYNC-START-TIME is the
time the request was sent, used for progress messages."
  (when (leman--sync-messages-p session)
    (message "Leman: Response arrived after %.2f seconds.  Reading %s JSON response..."
             (- (time-to-seconds) sync-start-time)
             (file-size-human-readable (buffer-size))))
  (let ((start-time (time-to-seconds)))
    (prog1 (leman--json-parse-buffer)
      (when (leman--sync-messages-p session)
        (message "Leman: Reading JSON took %.2f seconds"
                 (- (time-to-seconds) start-time))))))

(cl-defun leman--sync (session &key force quiet
                               (timeout 40) ;; Give the server an extra 10 seconds.
                               (filter leman-default-sync-filter))
  "Send sync request for SESSION.
If SESSION has a `next-batch' token, it's used.  If FORCE, first
delete any outstanding sync processes.  If QUIET, don't show a
message about syncing this time.  Cancel request after TIMEOUT
seconds.

FILTER may be an alist representing a raw event filter (i.e. not
a filter ID).  When unspecified, the value of
`leman-default-sync-filter' is used.  The filter is encoded with
`json-encode'.  To use no filter, specify FILTER as nil."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id257>.
  ;; TODO: Filtering: <https://matrix.org/docs/spec/client_server/r0.6.1#filtering>.
  ;; TODO: Use a filter ID for default filter.
  ;; TODO: Optionally, automatically sync again when HTTP request fails.
  ;; TODO: Ensure that the process in (map-elt leman-syncs session) is live.
  (leman--sync-maybe-interrupt session force)
  (pcase-let* (((cl-struct leman-session next-batch) session)
               (params (leman--sync-params next-batch filter))
               (sync-start-time (time-to-seconds))
               ;; FIXME: Auto-sync again in error handler.
               (process (leman-api session "sync" :params params
                          :timeout timeout
                          :then (apply-partially #'leman--sync-callback session)
                          :else (lambda (plz-error)
                                  (leman--sync-failed session timeout plz-error))
                          :json-read-fn (lambda ()
                                          (leman--sync-read-json session sync-start-time)))))
    (when process
      (setf (map-elt leman-syncs session) process)
      (when (and (not quiet) (leman--sync-messages-p session))
        (leman-message "Sync request sent.  Waiting for response...")))))

(defun leman--sync-callback (session data)
  "Process sync DATA for SESSION.
Runs `leman-sync-callback-hook' with SESSION."
  (leman-debug (leman-user-id (leman-session-user session)))
  ;; Remove the sync first.  We already have the data from it, and the
  ;; process has exited, so it's safe to run another one.
  (setf (map-elt leman-syncs session) nil)
  (pcase-let* (((map rooms ('next_batch next-batch) ('account_data (map ('events account-data-events))))
                data)
               ((map ('join joined-rooms) ('invite invited-rooms) ('leave left-rooms)) rooms)
               (num-events (+
                            ;; HACK: In `leman--push-joined-room-events', we do something
                            ;; with each event 3 times, so we multiply this by 3.
                            ;; FIXME: That calculation doesn't seem to be quite right, because
                            ;; the progress reporter never seems to hit 100% before it's done.
                            (* 3 (cl-loop for (_id . room) in joined-rooms
                                          sum (length (map-nested-elt room '(state events)))
                                          sum (length (map-nested-elt room '(timeline events)))))
                            (cl-loop for (_id . room) in invited-rooms
                                     sum (length (map-nested-elt room '(invite_state events)))))))
    ;; Append account data events.
    ;; TODO: Since only one event of each type is allowed in account data (the spec
    ;; doesn't seem to make this clear, but see
    ;; <https://github.com/matrix-org/matrix-js-sdk/blob/d0b964837f2820940bd93e718a2450b5f528bffc/src/store/memory.ts#L292>),
    ;; we should store account-data events in a hash table or alist rather than just a
    ;; list of events.
    (cl-callf2 append (cl-coerce account-data-events 'list) (leman-session-account-data session))
    ;; Process invited and joined rooms.
    (leman-with-progress-reporter (:when (leman--sync-messages-p session)
                                         :reporter ("Leman: Reading events..." 0 num-events))
      ;; Left rooms.
      (mapc (apply-partially #'leman--push-left-room-events session) left-rooms)
      ;; Invited rooms.
      (mapc (apply-partially #'leman--push-invite-room-events session) invited-rooms)
      ;; Joined rooms.
      (mapc (apply-partially #'leman--push-joined-room-events session) joined-rooms))
    ;; TODO: Process "left" rooms (remove room structs, etc).
    ;; NOTE: We update the next-batch token before updating any room buffers.  This means
    ;; that any errors in updating room buffers (like for unexpected event formats that
    ;; expose a bug) could cause events to not appear in the buffer, but the user could
    ;; still dismiss the error and start syncing again, and the client could remain
    ;; usable.  Updating the token after doing everything would be preferable in some
    ;; ways, but it would mean that an event that exposes a bug would be processed again
    ;; on every sync, causing the same error each time.  It would seem preferable to
    ;; maintain at least some usability rather than to keep repeating a broken behavior.
    (setf (leman-session-next-batch session) next-batch)
    ;; Run hooks which update buffers, etc.
    (run-hook-with-args 'leman-sync-callback-hook session)
    ;; Update the mode-line unread indicator.
    (leman--update-unread-indicator)
    ;; Show sync message if appropriate, and run after-initial-sync-hook.
    (when (leman--sync-messages-p session)
      (message (concat "Leman: Sync done."
                       (unless (leman-session-has-synced-p session)
                         (run-hook-with-args 'leman-after-initial-sync-hook session)
                         ;; Show tip after initial sync.
                         (setf (leman-session-has-synced-p session) t)
                         "  Use commands `leman-list-rooms' or `leman-view-room' to view a room."))))))

(defun leman--push-invite-room-events (session invited-room)
  "Push events for INVITED-ROOM into that room in SESSION."
  ;; TODO: Make leman-session-rooms a hash-table.
  (leman--push-joined-room-events session invited-room 'invite))

(defun leman--auto-sync (session)
  "If `leman-auto-sync' is non-nil, sync SESSION again."
  (when leman-auto-sync
    (leman--sync session)))

(defun leman--update-room-buffers (session)
  "Insert new events into SESSION's rooms which have buffers.
To be called in `leman-sync-callback-hook'."
  ;; TODO: Move this to leman-room.el, probably.
  ;; For now, we primitively iterate over the buffer list to find ones
  ;; whose mode is `leman-room-mode'.
  (let* ((buffers (cl-loop for room in (leman-session-rooms session)
                           for buffer = (map-elt (leman-room-local room) 'buffer)
                           when (buffer-live-p buffer)
                           collect buffer)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (save-window-excursion
          ;; NOTE: When the buffer has a window, it must be the selected one
          ;; while calling event-insertion functions.  I don't know if this is
          ;; due to a bug in EWOC or if I just misunderstand something, but
          ;; without doing this, events may be inserted at the wrong place.
          (when-let ((buffer-window (get-buffer-window buffer)))
            (select-window buffer-window))
          (cl-assert leman-room)
          (when (leman-room-ephemeral leman-room)
            ;; Ephemeral events.
            (leman-room--process-events (leman-room-ephemeral leman-room))
            (setf (leman-room-ephemeral leman-room) nil))
          (when-let ((new-events (alist-get 'new-events (leman-room-local leman-room))))
            ;; HACK: Process these events in reverse order, so that later events (like reactions)
            ;; which refer to earlier events can find them.  (Not sure if still necessary.)
            (leman-room--process-events (reverse new-events))
            (setf (alist-get 'new-events (leman-room-local leman-room)) nil))
          (when-let ((new-events (alist-get 'new-account-data-events (leman-room-local leman-room))))
            ;; Account data events.  Do this last so, e.g. read markers can refer to message events we've seen.
            (leman-room--process-events new-events)
            (setf (alist-get 'new-account-data-events (leman-room-local leman-room)) nil)))))))

(cl-defun leman--push-joined-room-events (session joined-room &optional (status 'join))
  "Push events for JOINED-ROOM into that room in SESSION.
Also used for left rooms, in which case STATUS should be set to
`leave'."
  (pcase-let* ((`(,id . ,event-types) joined-room)
               (id (symbol-name id)) ; Really important that the ID is a STRING!
               ;; TODO: Make leman-session-rooms a hash-table.
               (room (or (cl-find-if (lambda (room)
                                       (equal id (leman-room-id room)))
                                     (leman-session-rooms session))
                         (car (push (make-leman-room :id id) (leman-session-rooms session)))))
               ((map summary state ephemeral timeline
                     ('invite_state (map ('events invite-state-events)))
                     ('account_data (map ('events account-data-events)))
                     ('unread_notifications unread-notifications))
                event-types)
               (latest-timestamp))
    (setf (leman-room-status room) status
          (leman-room-unread-notifications room) unread-notifications)
    ;; NOTE: The idea is that, assuming that events in the sync response are in
    ;; chronological order, we push them to the lists in the room slots in that order,
    ;; leaving the head of each list as the most recent event of that type.  That means
    ;; that, e.g. the room state events may be searched in order to find, e.g. the most
    ;; recent room name event.  However, chronological order is not guaranteed, e.g. after
    ;; loading older messages (the "retro" function; this behavior is in development).

    ;; MAYBE: Use queue.el to store the events in a DLL, so they could
    ;; be accessed from either end.  Could be useful.

    ;; Push the StrippedState events to the room's invite-state.  (These events have no
    ;; timestamp data.)  We also run the event hook, because for invited rooms, the
    ;; invite-state events include room name, topic, etc.
    (cl-loop for event across invite-state-events
             for event-struct = (leman--make-event event)
             do (push event-struct (leman-room-invite-state room))
             (run-hook-with-args 'leman-event-hook event-struct room session))

    ;; Save room summary.
    (dolist (parameter '(m.heroes m.joined_member_count m.invited_member_count))
      (when (alist-get parameter summary)
        ;; These fields are only included when they change.
        (setf (alist-get parameter (leman-room-summary room)) (alist-get parameter summary))))

    ;; Update account data.  According to the spec, only one of each event type is
    ;; supposed to be present in a room's account data, so we store them as an alist keyed
    ;; on their type.  (NOTE: We don't currently make them into event structs, but maybe
    ;; we should in the future.)
    (cl-loop for event across account-data-events
             for type = (alist-get 'type event)
             do (setf (alist-get type (leman-room-account-data room) nil nil #'equal) event))
    ;; But we also need to track just the new events so we can process those in a room
    ;; buffer (and for some reason, we do make them into structs here, but I don't
    ;; remember why).  FIXME: Unify this.
    (cl-callf2 append (mapcar #'leman--make-event account-data-events)
               (alist-get 'new-account-data-events (leman-room-local room)))

    ;; Push the new state and timeline events to the room's slots,
    ;; collecting their 'leman-event' structs (in their original order)
    ;; for running hooks below.
    (cl-macrolet ((push-events (type accessor)
                    ;; Push new events of TYPE to room's slot of ACCESSOR.
                    ;; Return a list of the event structs and the latest
                    ;; origin-server-ts pushed.
                    `(let ((ts 0) (event-structs nil))
                       (cl-loop for event across-ref (alist-get 'events ,type)
                                do (setf event (leman--make-event event))
                                (push event event-structs)
                                (push event (,accessor room))
                                (when (leman--sync-messages-p session)
                                  (leman-progress-update))
                                (when (> (leman-event-origin-server-ts event) ts)
                                  (setf ts (leman-event-origin-server-ts event))))
                       ;; One would think that one should use `maximizing' here, but, completely
                       ;; inexplicably, it sometimes returns nil, even when every single value it's comparing
                       ;; is a number.  It's absolutely bizarre, but I have to do the equivalent manually.
                       (list (nreverse event-structs) ts))))
      (pcase-let* ((`(,state-event-structs ,state-ts)
                    (push-events state leman-room-state))
                   (`(,timeline-event-structs ,timeline-ts)
                    (push-events timeline leman-room-timeline)))
        (setf latest-timestamp (max state-ts timeline-ts))
        ;; NOTE: We also append the new events to the new-events list in the room's local
        ;; slot, which is used by `leman--update-room-buffers' to insert only new events.
        ;; FIXME: Does this also need to be done for invite-state events?
        (cl-callf2 append timeline-event-structs
                   (alist-get 'new-events (leman-room-local room)))
        ;; Update room's latest-timestamp slot.
        (when (> latest-timestamp (or (leman-room-latest-ts room) 0))
          (setf (leman-room-latest-ts room) latest-timestamp))
        (unless (leman-session-has-synced-p session)
          ;; Only set this token on initial sync, otherwise it would
          ;; overwrite earlier tokens from loading earlier messages.
          (setf (leman-room-prev-batch room) (alist-get 'prev_batch timeline)))
        ;; Run event hook for state and timeline events.
        (dolist (event-structs (list state-event-structs timeline-event-structs))
          (dolist (event event-structs)
            (run-hook-with-args 'leman-event-hook event room session)
            (when (leman--sync-messages-p session)
              (leman-progress-update))))))
    ;; Ephemeral events (do this after state and timeline hooks, so those events will be
    ;; in the hash tables).
    (cl-loop for event across (alist-get 'events ephemeral)
             for event-struct = (leman--make-event event)
             do (push event-struct (leman-room-ephemeral room))
             (leman--process-event event-struct room session))
    (when (leman-session-has-synced-p session)
      ;; NOTE: We don't fill gaps in "limited" requests on initial
      ;; sync, only in subsequent syncs, e.g. after the system has
      ;; slept and awakened.
      ;; NOTE: When not limited, the read value is `:json-false', so
      ;; we must explicitly compare to t.
      (when (eq t (alist-get 'limited timeline))
	;; Timeline was limited: start filling gap.  We start the
	;; gap-filling, retrieving up to the session's current
	;; next-batch token (this function is not called when retrieving
	;; older messages, so the session's next-batch token is only
	;; evaluated once, when this chain begins, and then that token
	;; is passed to repeated calls to `leman-room-retro-to-token'
	;; until the gap is filled).
	(leman-room-retro-to-token room session (alist-get 'prev_batch timeline)
				   (leman-session-next-batch session))))))
(defun leman--push-left-room-events (session left-room)
  "Push events for LEFT-ROOM into that room in SESSION."
  (leman--push-joined-room-events session left-room 'leave))

(defun leman--make-event (event)
  "Return `leman-event' struct for raw EVENT list.
Adds sender to `leman-users' when necessary."
  (pcase-let* (((map content type unsigned redacts
                     ('event_id id) ('origin_server_ts ts)
                     ('sender sender-id) ('state_key state-key))
                event)
               (sender (or (gethash sender-id leman-users)
                           (puthash sender-id (make-leman-user :id sender-id)
                                    leman-users))))
    ;; MAYBE: Handle other keys in the event, such as "room_id" in "invite" events.
    (make-leman-event :id id :sender sender :type type :content content :state-key state-key
                      :origin-server-ts ts :unsigned unsigned
                      ;; Since very few events will be redactions and have this key, we
                      ;; record it in the local slot alist rather than as another slot on
                      ;; the struct.
                      :local (when redacts
                               (leman-alist 'redacts redacts)))))

(defun leman--put-event (event _room session)
  "Put EVENT on SESSION's events table."
  (puthash (leman-event-id event) event (leman-session-events session)))

;; FIXME: These functions probably need to compare timestamps to
;; ensure that older events that are inserted at the head of the
;; events lists aren't used instead of newer ones.

;; TODO: These two functions should be folded into event handlers.

;;;;; Reading/writing sessions

;; TODO: Use `persist' and/or `multisession'.

(defun leman--read-sessions ()
  "Return saved sessions alist read from disk.
Returns nil if unable to read `leman-sessions-file'."
  (cl-labels ((plist-to-session (plist)
                (pcase-let* (((map (:user user-data) (:server server-data)
                                   (:token token) (:transaction-id transaction-id))
                              plist)
                             (user (apply #'make-leman-user user-data))
                             (server (apply #'make-leman-server server-data))
                             (session (make-leman-session :user user :server server
                                                          :token token :transaction-id transaction-id)))
                  (setf (leman-session-events session) (make-hash-table :test #'equal))
                  session)))
    (when (file-exists-p leman-sessions-file)
      (pcase-let* ((read-circle t)
                   (sessions (with-temp-buffer
                               (insert-file-contents leman-sessions-file)
                               (read (current-buffer)))))
        (prog1
            (cl-loop for (id . plist) in sessions
                     collect (cons id (plist-to-session plist)))
          (message "Leman: Read sessions."))))))

(defun leman--write-sessions (sessions-alist)
  "Write SESSIONS-ALIST to disk."
  ;; We only record the slots we need.  We record them as a plist
  ;; so that changes to the struct definition don't matter.
  ;; NOTE: If we ever persist more session data (like room data, so we
  ;; could avoid doing an initial sync next time), we should limit the
  ;; amount of session data saved (e.g. room history could grow
  ;; forever on-disk, which probably isn't what we want).

  ;; NOTE: This writes all current sessions, even if there are multiple active ones and only one
  ;; is being disconnected.  That's probably okay, but it might be something to keep in mind.
  (cl-labels ((session-plist (session)
                (pcase-let* (((cl-struct leman-session user server token transaction-id) session)
                             ((cl-struct leman-user (id user-id) username) user)
                             ((cl-struct leman-server (name server-name) uri-prefix) server))
                  (list :user (list :id user-id
                                    :username username)
                        :server (list :name server-name
                                      :uri-prefix uri-prefix)
                        :token token
                        :transaction-id transaction-id))))
    (message "Leman: Writing sessions...")
    (with-temp-file leman-sessions-file
      (pcase-let* ((print-level nil)
                   (print-length nil)
                   ;; Very important to use `print-circle', although it doesn't
                   ;; solve everything.  Writing/reading Lisp data can be tricky...
                   (print-circle t)
                   (sessions-alist-plist (cl-loop for (id . session) in sessions-alist
                                                  collect (cons id (session-plist session)))))
        (prin1 sessions-alist-plist (current-buffer))))
    ;; Ensure permissions are safe.
    (chmod leman-sessions-file #o600)))

(defun leman--kill-emacs-hook ()
  "Function to be added to `kill-emacs-hook'.
Writes Leman session to disk when enabled."
  (ignore-errors
    ;; To avoid interfering with Emacs' exit, We must be careful that
    ;; this function handles errors, so just ignore any.
    (when (and leman-save-sessions
               leman-sessions)
      (leman--write-sessions leman-sessions))))

;;;;; Event handlers

(defvar leman-event-handlers nil
  "Alist mapping event types to functions which process an event of each type.
Each function is called with three arguments: the event, the
room, and the session.  These handlers are run regardless of
whether a room has a live buffer.")

(defun leman--process-event (event room session)
  "Process EVENT for ROOM in SESSION.
Uses handlers defined in `leman-event-handlers'.  If no handler
is defined for EVENT's type, does nothing and returns nil.  Any
errors signaled during processing are demoted in order to prevent
unexpected errors from arresting event processing and syncing."
  (when-let ((handler (alist-get (leman-event-type event) leman-event-handlers nil nil #'equal)))
    ;; We demote any errors that happen while processing events, because it's possible for
    ;; events to be malformed in unexpected ways, and that could cause an error, which
    ;; would stop processing of other events and prevent further syncing.  See,
    ;; e.g. <https://github.com/alphapapa/ement.el/pull/61>.
    (with-demoted-errors "Leman (leman--process-event): Error processing event: %S"
      (funcall handler event room session))))

(defmacro leman-defevent (type &rest body)
  "Define an event handling function for events of TYPE, a string.
Around the BODY, the variable `event' is bound to the event being
processed, `room' to the room struct in which the event occurred,
and `session' to the session.  Adds function to
`leman-event-handlers', which see."
  (declare (indent defun))
  `(setf (alist-get ,type leman-event-handlers nil nil #'string=)
         (lambda (event room session)
           ,(concat "`leman-' handler function for " type " events.")
           ,@body)))

;; I love how Lisp macros make it so easy and concise to define these
;; event handlers!

(leman-defevent "m.room.avatar"
  (when leman-room-avatars
    ;; If room avatars are disabled, we don't download avatars at all.  This
    ;; means that, if a user has them disabled and then reenables them, they will
    ;; likely need to reconnect to cause them to be displayed in most rooms.
    (if-let ((url (alist-get 'url (leman-event-content event))))
        (plz-run
         (plz-queue leman-images-queue
           'get (leman--mxc-to-url url session) :as 'binary :noquery t
           :then (lambda (data)
                   (when leman-room-avatars
                     ;; MAYBE: Store the raw image data instead of using create-image here.
                     (let ((image (create-image data nil 'data-p
                                                :ascent 'center
                                                :max-width leman-room-avatar-max-width
                                                :max-height leman-room-avatar-max-height)))
                       (if (not image)
                           (progn
                             (display-warning 'leman (format "Room avatar seems unreadable:  ROOM-ID:%S  AVATAR-URL:%S"
                                                             (leman-room-id room) (leman--mxc-to-url url session)))
                             (setf (leman-room-avatar room) nil
                                   (alist-get 'room-list-avatar (leman-room-local room)) nil))
                         (when (fboundp 'imagemagick-types)
                           ;; Only do this when ImageMagick is supported.
                           ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
                           (setf (image-property image :type) 'imagemagick))
                         ;; We set the room-avatar slot to a propertized string that
                         ;; displays as the image.  This seems the most convenient thing to
                         ;; do.  We also unset the cached room-list-avatar so it can be
                         ;; remade.
                         (setf (leman-room-avatar room) (propertize " " 'display image)
                               (alist-get 'room-list-avatar (leman-room-local room)) nil)))))))
      ;; Unset avatar.
      (setf (leman-room-avatar room) nil
            (alist-get 'room-list-avatar (leman-room-local room)) nil))))

(leman-defevent "m.room.create"
  (ignore session)
  (pcase-let* (((cl-struct leman-event (content (map type))) event))
    (when type
      (setf (leman-room-type room) type))))

(leman-defevent "m.room.member"
  "Put/update member on `leman-users' and room's members table."
  (ignore session)
  (pcase-let* (((cl-struct leman-room members) room)
               ((cl-struct leman-event state-key
                           (content (map displayname membership
                                         ('avatar_url avatar-url))))
                event)
               (user (or (gethash state-key leman-users)
                         (puthash state-key
                                  (make-leman-user
                                   :id state-key :avatar-url avatar-url
                                   ;; NOTE: The spec doesn't seem to say whether the
                                   ;; displayname in the member event applies only to
                                   ;; the room or is for the user generally, so we'll
                                   ;; save it in the struct anyway.
                                   ;; FIXME: This is probably wrong: it probably means
                                   ;; overwriting the global displayname with any
                                   ;; room-specific one that was most recently processed.
                                   :displayname displayname)
                                  leman-users))))
    (pcase membership
      ("join"
       (puthash state-key user members)
       (if displayname
           ;; NOTE: This handler is only called for new events, not when retrieving old events.
           ;; Therefore it's safe to update the cached displayname from such an event.
           (puthash user displayname (leman-room-displaynames room))
         ;; No displayname set for this room: recalculate.
         (leman--user-displayname-in room user 'recalculate)))
      (_ (remhash state-key members)
         (remhash user (leman-room-displaynames room))))))

(leman-defevent "m.room.name"
  (ignore session)
  (pcase-let* (((cl-struct leman-event (content (map name))) event))
    (when name
      ;; Recalculate room name and cache in slot.
      (setf (leman-room-display-name room) (leman--room-display-name room)))))

(leman-defevent "m.room.topic"
  (ignore session)
  (pcase-let* (((cl-struct leman-event (content (map topic))) event))
    (when topic
      (setf (leman-room-topic room) topic))))

(leman-defevent "m.receipt"
  (ignore session)
  (pcase-let (((cl-struct leman-event content) event)
              ((cl-struct leman-room (receipts room-receipts)) room))
    (cl-loop for (event-id . receipts) in content
             do (cl-loop for (user-id . receipt) in (alist-get 'm.read receipts)
                         ;; Users may not have been "seen" yet, so although we'd
                         ;; prefer to key on the user struct, we key on the user ID.
                         ;; Same for events, unfortunately.
                         ;; NOTE: The JSON map keys are converted to symbols by `json-read'.
                         ;; MAYBE: (Should we keep them that way?  It would use less memory, I guess.)
                         do (puthash (symbol-name user-id)
                                     (cons (symbol-name event-id) (alist-get 'ts receipt))
                                     room-receipts)))))

(leman-defevent "m.space.child"
  ;; SPEC: v1.2/11.35.
  (pcase-let* ((space-room room)
               ((cl-struct leman-session rooms) session)
               ((cl-struct leman-room (id parent-room-id)) space-room)
               ((cl-struct leman-event (state-key child-room-id) (content (map via))) event)
               (child-room (cl-find child-room-id rooms :key #'leman-room-id :test #'equal)))
    (if via
        ;; Child being declared: add it.
        (progn
          (cl-pushnew child-room-id (alist-get 'children (leman-room-local space-room)) :test #'equal)
          (when child-room
            ;; The user is also in the child room: link the parent space-room in it.
            ;; FIXME: On initial sync, if the child room hasn't been processed yet, this will fail.
            (cl-pushnew parent-room-id (alist-get 'parents (leman-room-local child-room)) :test #'equal)))
      ;; Child being disowned: remove it.
      (setf (alist-get 'children (leman-room-local space-room))
            (delete child-room-id (alist-get 'children (leman-room-local space-room))))
      (when child-room
        ;; The user is also in the child room: unlink the parent space-room in it.
        (setf (alist-get 'parents (leman-room-local child-room))
              (delete parent-room-id (alist-get 'parents (leman-room-local child-room))))))))

(leman-defevent "m.room.canonical_alias"
  (ignore session)
  (pcase-let (((cl-struct leman-event (content (map alias))) event))
    (setf (leman-room-canonical-alias room) alias)))

(defun leman--link-children (session)
  "Link child rooms in SESSION.
To be called after initial sync."
  ;; On initial sync, when processing m.space.child events, the child rooms may not have
  ;; been processed yet, so we link them again here.
  (pcase-let (((cl-struct leman-session rooms) session))
    (dolist (room rooms)
      (pcase-let (((cl-struct leman-room (id parent-id) (local (map children))) room))
        (when children
          (dolist (child-id children)
            (when-let ((child-room (cl-find child-id rooms :key #'leman-room-id :test #'equal)))
              (cl-pushnew parent-id (alist-get 'parents (leman-room-local child-room)) :test #'equal))))))))

;;;;; Transient

(require 'transient)

;; These files are not required by leman.el (leman-tabulated-room-list
;; requires leman, so it cannot be required here), but their commands
;; are autoloaded.
(declare-function leman-list-rooms "leman-room-list")
(declare-function leman-tabulated-room-list "leman-tabulated-room-list")
(declare-function leman-directory "leman-directory")

;;;###autoload
(transient-define-prefix leman-transient ()
  "Transient for Leman, callable from any buffer."
  [:pad-keys t
             ["Session"
              ("c" "Connect" leman-connect)
              ("d" "Disconnect" leman-disconnect)
              ("K" "Kill Leman buffers" leman-kill-buffers)
              ("P" "Set display name" leman-set-display-name)
              ("S" "Sync now" leman-room-sync)]
             ["Rooms"
              ("l" "List rooms" leman-list-rooms)
              ("t" "List rooms (tabulated)" leman-tabulated-room-list)
              ("v" "View room" leman-view-room)
              ("j" "Join room" leman-room-join)
              ("N" "Create room" leman-create-room)
              ("V" "View space" leman-view-space)]]
  [:pad-keys t
             ["Room actions"
              ("i" "Invite user" leman-invite-user)
              ("T" "Set topic" leman-room-set-topic)
              ("f" "Tag room" leman-tag-room)
              ("s" "Set notification state" leman-room-set-notification-state)
              ("m" "Mark read to point" leman-room-mark-read)
              ("L" "Leave room" leman-room-leave)
              ("F" "Forget room" leman-forget-room)]
             ["Notifications"
              ("n" "Notifications" leman-notifications)
              ("M" "Mentions" leman-notify-switch-to-mentions-buffer)
              ("B" "Notifications buffer" leman-notify-switch-to-notifications-buffer)
              ("u" "Ignore user" leman-ignore-user)]]
  [:pad-keys t
             ["Misc"
              ("D" "Room directory" leman-directory)
              ("o" "Occur search in room" leman-room-occur)
              ("r" "Room transient" leman-room-transient)
              ("C" "Flush colors" leman-room-flush-colors)
              ("q" "Quit" transient-quit-one)]])

;;;;; Unread indicator

(defvar leman-unread-indicator-string nil
  "String shown in the mode line by `leman-unread-indicator-mode'.
Updated by `leman--update-unread-indicator'.")

(defun leman--unread-counts ()
  "Return cons of (NOTIFICATIONS . HIGHLIGHTS) unread counts.
Counts are summed over joined rooms in all sessions.  They are
the server-computed values, which account for each room's
notification rules."
  (let ((notifications 0)
        (highlights 0))
    (cl-loop for (_id . session) in leman-sessions
             do (cl-loop for room in (leman-session-rooms session)
                         when (eq 'join (leman-room-status room))
                         do (pcase-let (((map notification_count highlight_count)
                                         (leman-room-unread-notifications room)))
                              (cl-incf notifications (or notification_count 0))
                              (cl-incf highlights (or highlight_count 0)))))
    (cons notifications highlights)))

(defun leman--unread-help-echo ()
  "Return a help-echo string summarizing rooms with unread counts."
  (string-join
   (cl-loop for (_id . session) in leman-sessions
            append (cl-loop for room in (leman-session-rooms session)
                            for notifications = (map-elt (leman-room-unread-notifications room)
                                                         'notification_count 0)
                            when (and (eq 'join (leman-room-status room))
                                      (> notifications 0))
                            collect (format "%s: %d%s"
                                            (or (leman-room-display-name room)
                                                (leman-room-id room))
                                            notifications
                                            (if-let ((highlights (map-elt (leman-room-unread-notifications room)
                                                                          'highlight_count 0)))
                                                (format " (%d highlights)" highlights)
                                              "")))
            into lines
            finally return lines)
   "\n"))

(defun leman--update-unread-indicator ()
  "Update `leman-unread-indicator-string'.
To be called after syncs and when read markers are moved."
  (setf leman-unread-indicator-string
        (if leman-sessions
            (pcase-let ((`(,notifications . ,highlights) (leman--unread-counts)))
              (when (or (> notifications 0) (> highlights 0))
                (propertize
                 (concat (when (> notifications 0)
                           (propertize (format "L:%d" notifications) 'face 'bold))
                         (when (> highlights 0)
                           (propertize (format "(%d)" highlights) 'face 'leman-room-mention)))
                 'help-echo (leman--unread-help-echo))))
          "")))

(define-minor-mode leman-unread-indicator-mode
  "Show unread notification counts in the mode line.
Counts are updated after each sync and when read markers are
moved.  Highlights (i.e. mentions) are shown in parentheses."
  :global t
  :group 'leman
  (if leman-unread-indicator-mode
      (progn
        (add-to-list 'global-mode-string 'leman-unread-indicator-string)
        (leman--update-unread-indicator))
    (setq global-mode-string (delq 'leman-unread-indicator-string global-mode-string))
    (setf leman-unread-indicator-string nil)))

;;;;; Savehist compatibility

;; See <https://github.com/alphapapa/ement.el/issues/216>.

(defvar savehist-save-hook)

(with-eval-after-load 'savehist
  ;; TODO: Consider using a symbol property on our commands and checking that rather than
  ;; symbol names; would avoid consing.
  (defun leman--savehist-save-hook ()
    "Remove all `leman-' commands from `command-history'.
Because when `savehist' saves `command-history', it includes the
interactive arguments passed to the command, which in our case
includes large data structures that should never be persisted!"
    (setf command-history
          (cl-remove-if (pcase-lambda (`(,command . ,_))
                          (cl-typecase command
                            (symbol (string-match-p (rx bos "leman-") (symbol-name command)))))
                        command-history)))
  (cl-pushnew 'leman--savehist-save-hook savehist-save-hook))

;;;; Footer

(provide 'leman)

;;; leman.el ends here
