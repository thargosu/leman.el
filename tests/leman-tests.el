;;; leman-tests.el --- Tests for Leman.el                  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'ert)
(require 'map)

(require 'leman-lib)
(require 'leman-room)
(require 'leman-room-list)
(require 'leman-tabulated-room-list)

;; Variables from leman.el, which the tests don't load.
(defvar leman-users)

;;;; Helpers

(defun leman-tests--member-event (id state-key old new &optional kicked-p avatar-differs-p)
  "Return a membership event for testing.
OLD and NEW are the previous and new membership strings.  KICKED-P
means the sender differs from the state-key (i.e. another user
did the action).  AVATAR-DIFFERS-P makes the event's new avatar
URL differ from the previous one."
  (make-leman-event
   :id (format "%s-%s" id state-key)
   :state-key state-key
   :sender (make-leman-user :id (if kicked-p "@admin:example.com" state-key))
   :origin-server-ts id
   :type "m.room.member"
   :content `((membership . ,new)
              (avatar_url . ,(if avatar-differs-p "avatar-new" "avatar-same"))
              (displayname . nil))
   :unsigned `((prev_content . ((membership . ,old)
                                (avatar_url . "avatar-same")
                                (displayname . nil))))))

;;;; Tests

(ert-deftest leman--format-body-mentions ()
  (let ((room (make-leman-room
               :members (map-into
                         `(("@foo:matrix.org" . ,(make-leman-user :id "@foo:matrix.org"
                                                                  :displayname "foo"))
                           ("@bar:matrix.org" . ,(make-leman-user :id "@bar:matrix.org"
                                                                  :displayname "bar")))
                         '(hash-table :test equal)))))
    (should (equal (leman--format-body-mentions "@foo: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (leman--format-body-mentions "@foo:matrix.org: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (leman--format-body-mentions "foo: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (leman--format-body-mentions "@foo and @bar:matrix.org: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a> and <a href=\"https://matrix.to/#/@bar:matrix.org\">bar</a>: hi"))
    (should (equal (leman--format-body-mentions "foo: how about you and @bar ..." room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: how about you and <a href=\"https://matrix.to/#/@bar:matrix.org\">bar</a> ..."))
    (should (equal (leman--format-body-mentions "Hello, @foo:matrix.org." room)
                   "Hello, <a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>."))
    (should (equal (leman--format-body-mentions "Hello, @foo:matrix.org, how are you?" room)
                   "Hello, <a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>, how are you?"))))

(ert-deftest leman-room--pair-events ()
  "Test pairing of membership events by state-key."
  (let* ((join-alice (leman-tests--member-event 1 "@alice:example.com" nil "join"))
         (join-bob (leman-tests--member-event 2 "@bob:example.com" nil "join"))
         (leave-alice (leman-tests--member-event 3 "@alice:example.com" "join" "leave"))
         (leave-bob (leman-tests--member-event 4 "@bob:example.com" "join" "leave"))
         (leave-erin (leman-tests--member-event 5 "@erin:example.com" "join" "leave")))
    ;; Paired events are returned in the events' order.
    (should (equal (leman-room--pair-events (list join-alice join-bob)
                                            (list leave-alice leave-bob))
                   (list (list leave-alice leave-bob) nil nil)))
    ;; Unpaired events remain in their respective lists.
    (should (equal (leman-room--pair-events (list join-alice)
                                            (list leave-bob leave-erin))
                   (list nil (list join-alice) (list leave-bob leave-erin))))
    ;; An OTHERS event's state-key is consumed once, so later EVENTS
    ;; events having that state-key are dropped.
    (let ((join-alice-again (leman-tests--member-event 6 "@alice:example.com" "invite" "join")))
      (should (equal (leman-room--pair-events (list join-alice join-alice-again)
                                              (list leave-alice))
                     (list (list leave-alice) nil nil))))))

(ert-deftest leman-room--format-membership-events ()
  "Test membership events summary formatting."
  ;; `leman-users' is declared without a value in leman-room.el (its real
  ;; definition is in leman.el, which the tests don't load); SET it directly
  ;; because SET is not lexically scoped, so the formatter's dynamic lookup
  ;; will find it.
  (set 'leman-users (make-hash-table :test #'equal))
  (let* ((room (make-leman-room :id "!room:example.com"))
         (leman-room room)
         (format-summary (lambda (&rest events)
                           (substring-no-properties
                            (leman-room--format-membership-events
                             (make-leman-room-membership-events :events events)
                             room)))))
    ;; A single event is formatted by `leman-room--format-member-event'.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "join"))
                   "@alice:example.com joined"))
    ;; Users are listed in events order within a category.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "join")
                            (leman-tests--member-event 2 "@bob:example.com" nil "join")
                            (leman-tests--member-event 3 "@carol:example.com" nil "join"))
                   "Membership: 3 joined (@alice:example.com, @bob:example.com, @carol:example.com)."))
    ;; Categories are listed in a fixed order.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@carol:example.com" nil "join")
                            (leman-tests--member-event 2 "@bob:example.com" "join" "leave")
                            (leman-tests--member-event 3 "@alice:example.com" nil "join")
                            (leman-tests--member-event 4 "@dave:example.com" "join" "leave"))
                   "Membership: 2 joined (@carol:example.com, @alice:example.com); 2 left (@bob:example.com, @dave:example.com)."))
    ;; A join followed by a leave is counted as "joined and left".
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "join")
                            (leman-tests--member-event 2 "@alice:example.com" "join" "leave"))
                   "Membership: 1 joined and left (@alice:example.com)."))
    ;; Events that are both joined and rejoined are counted as rejoined.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "join")
                            (leman-tests--member-event 2 "@alice:example.com" "leave" "join"))
                   "Membership: 1 rejoined (@alice:example.com)."))
    ;; A kick followed by a rejoin is not also counted as leaving.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "leave" 'kicked-p)
                            (leman-tests--member-event 2 "@alice:example.com" "leave" "join"))
                   "Membership: 1 was kicked and rejoined (@alice:example.com)."))
    ;; A single kick event is formatted by `leman-room--format-member-event'.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "leave" 'kicked-p))
                   "@admin:example.com kicked @alice:example.com"))
    ;; A rejoin followed by a leave.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "leave" "join")
                            (leman-tests--member-event 2 "@alice:example.com" "join" "leave"))
                   "Membership: 1 rejoined and left (@alice:example.com)."))
    ;; Invitations.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "leave" "invite")
                            (leman-tests--member-event 2 "@bob:example.com" "leave" "invite"))
                   "Membership: 2 invited (@alice:example.com, @bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "invite" "leave")
                            (leman-tests--member-event 2 "@bob:example.com" "invite" "leave"))
                   "Membership: 2 rejected invitation (@alice:example.com, @bob:example.com)."))
    ;; Bans and unbans.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "leave" "ban")
                            (leman-tests--member-event 2 "@bob:example.com" "invite" "ban"))
                   "Membership: 2 banned (@alice:example.com, @bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "ban" "leave")
                            (leman-tests--member-event 2 "@bob:example.com" "ban" "leave"))
                   "Membership: 2 unbanned (@alice:example.com, @bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "ban" 'kicked-p)
                            (leman-tests--member-event 2 "@bob:example.com" "join" "ban" 'kicked-p))
                   "Membership: 2 kicked and banned (@alice:example.com, @bob:example.com)."))
    ;; Ban transitions which the summary does not classify are omitted.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "ban")
                            (leman-tests--member-event 2 "@bob:example.com" "join" "leave"))
                   "Membership: 1 left (@bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "ban" "ban")
                            (leman-tests--member-event 2 "@bob:example.com" "join" "leave"))
                   "Membership: 1 left (@bob:example.com)."))
    ;; Name and avatar changes.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "join")
                            (leman-tests--member-event 2 "@bob:example.com" "join" "join"))
                   "Membership: 2 changed name (@alice:example.com, @bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "join" nil 'avatar-differs-p)
                            (leman-tests--member-event 2 "@bob:example.com" "join" "join" nil 'avatar-differs-p))
                   "Membership: 2 changed avatar (@alice:example.com, @bob:example.com)."))
    ;; Unclassifiable events are omitted.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "leave" "knock")
                            (leman-tests--member-event 2 "@bob:example.com" nil "join"))
                   "Membership: 1 joined (@bob:example.com)."))
    ;; The summary type is propertized with the bold face.
    (let* ((raw (leman-room--format-membership-events
                 (make-leman-room-membership-events
                  :events (list (leman-tests--member-event 1 "@alice:example.com" nil "join")
                                (leman-tests--member-event 2 "@bob:example.com" nil "join")))
                 room))
           (pos (string-search "joined" raw)))
      (should (eq 'bold (get-text-property pos 'face raw))))))

(ert-deftest leman-room--initial-header ()
  "Test initial room buffer header."
  (let ((plain (make-leman-room :id "!room:example.com"))
        (encrypted (make-leman-room :id "!room:example.com"
                                    :state (list (make-leman-event :type "m.room.encryption"))))
        (encrypted-invite (make-leman-room :id "!room:example.com"
                                           :invite-state (list (make-leman-event :type "m.room.encryption")))))
    ;; A plain room has an empty header.
    (should (string-empty-p (leman-room--initial-header plain)))
    ;; An encrypted room's header warns, whether encryption is in the
    ;; state or the invite state.
    (should (string-search "encrypted room"
                           (substring-no-properties (leman-room--initial-header encrypted))))
    (should (string-search "encrypted room"
                           (substring-no-properties (leman-room--initial-header encrypted-invite))))
    (should (eq 'font-lock-warning-face
                (get-text-property 0 'face (leman-room--initial-header encrypted))))))

(ert-deftest leman-room--initial-footer ()
  "Test initial room buffer footer."
  (let ((plain (make-leman-room :id "!room:example.com"))
        (invited (make-leman-room :id "!room:example.com" :status 'invite))
        (space (make-leman-room :id "!room:example.com" :type "m.space"))
        (invited-space (make-leman-room :id "!room:example.com" :status 'invite :type "m.space")))
    ;; A plain room has an empty footer.
    (should (string-empty-p (leman-room--initial-footer plain)))
    ;; An invited room's footer offers to join.
    (let ((footer (leman-room--initial-footer invited))
          (pos (string-search "[Join this room]" (leman-room--initial-footer invited))))
      (should (string-search "invited to this room" (substring-no-properties footer)))
      (should pos)
      (should (get-text-property pos 'button footer))
      (should (functionp (get-text-property pos 'action footer))))
    ;; A space's footer offers to view its rooms.
    (let ((footer (leman-room--initial-footer space))
          (pos (string-search "[View rooms in this space]" (leman-room--initial-footer space))))
      (should (string-search "grouping of other rooms" (substring-no-properties footer)))
      (should pos)
      (should (get-text-property pos 'button footer))
      (should (functionp (get-text-property pos 'action footer))))
    ;; For an invited space, the invitation takes precedence.
    (should (string-search "invited to this room"
                           (substring-no-properties (leman-room--initial-footer invited-space))))))

(defun leman-tests--taxy-items (taxy)
  "Return all of TAXY's items, including those in its sub-taxys."
  (append (taxy-items taxy)
          (cl-loop for sub-taxy in (taxy-taxys taxy)
                   append (leman-tests--taxy-items sub-taxy))))

(ert-deftest leman-room-list--build-taxy ()
  "Test building the room list taxy."
  (let* ((room-old (make-leman-room :id "!old:example.com" :latest-ts 100))
         (room-new (make-leman-room :id "!new:example.com" :latest-ts 200))
         (session (make-leman-session :user (make-leman-user :id "@me:example.com")))
         (taxy (leman-room-list--build-taxy
                (list (vector room-old session) (vector room-new session))
                leman-room-list-default-keys
                #'identity))
         (items (mapcar (lambda (item) (elt item 0))
                        (leman-tests--taxy-items taxy))))
    (should (equal "Leman Rooms" (taxy-name taxy)))
    (should (member room-new items))
    (should (member room-old items))
    ;; Rooms are sorted latest-first.
    (should (< (cl-position room-new items :test #'equal)
               (cl-position room-old items :test #'equal)))))

(ert-deftest leman-tabulated-room-list--entry ()
  "Test building a tabulated room list entry."
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:example.com")))
         (room (make-leman-room :id "!room:example.com"))
         (entry (leman-tabulated-room-list--entry session room))
         (name (elt (elt entry 1) 5)))
    ;; The entry identifies the room and has one column per format.
    (should (eq room (car entry)))
    (should (= (length (elt entry 1)) 10))
    ;; A plain room's name has only the base face.
    (should (equal '(:inherit (leman-tabulated-room-list-name))
                   (get-text-property 0 'face (car name))))))

(ert-deftest leman-tabulated-room-list--entry-membership-faces ()
  "Test that invited and left rooms are face-modified.
This checks the 'leave branch, which upstream ement.el malformed
by passing the arguments to `cons' in reverse, and the
`leman-room-status' slot, which upstream read from the wrong
slot, so these branches never applied."
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:example.com")))
         (invited-room (make-leman-room :id "!invited:example.com" :status 'invite))
         (left-room (make-leman-room :id "!left:example.com" :status 'leave))
         (invited-entry (leman-tabulated-room-list--entry session invited-room))
         (left-entry (leman-tabulated-room-list--entry session left-room))
         (invited-name (car (elt (elt invited-entry 1) 5)))
         (left-name (car (elt (elt left-entry 1) 5))))
    ;; Topics are prefixed, and the name's face inherits the membership face.
    (should (string-search "[invited]"
                           (substring-no-properties (elt (elt invited-entry 1) 6))))
    (should (member 'leman-tabulated-room-list-invited
                    (map-elt (get-text-property 0 'face invited-name) :inherit)))
    (should (string-search "[left]"
                           (substring-no-properties (elt (elt left-entry 1) 6))))
    (should (member 'leman-tabulated-room-list-left
                    (map-elt (get-text-property 0 'face left-name) :inherit)))))

(ert-deftest leman-room--render-html-spoilers ()
  "Test that Matrix spoilers are rendered and hidden.
Both the valueless attribute form (as sent by, e.g. Element's
/spoiler command) and the reasoned form are covered."
  (let ((string (let ((leman-room-use-variable-pitch nil))
                  (leman-room--render-html
                   "Before <span data-mx-spoiler>the secret</span> mid <span data-mx-spoiler=\"plot twist\">snape did it</span>"
                   nil))))
    ;; Labels and content are rendered in order.
    (should (string-match "\\[spoiler\\] the secret" string))
    (should (string-match "\\[spoiler: plot twist\\] snape did it" string))
    ;; Content is hidden and toggleable.
    (let ((cbeg (next-single-property-change 0 'leman-spoiler-content string)))
      (should cbeg)
      (should (eq (get-text-property cbeg 'invisible string) 'leman-spoiler))
      (should (get-text-property cbeg 'keymap string))
      (should (equal (substring-no-properties string cbeg
                                               (next-single-property-change cbeg 'leman-spoiler-content string))
                     "the secret"))
      ;; Labels are clickable too.
      (let ((label-beg (string-match "\\[spoiler\\]" string)))
        (should (get-text-property label-beg 'keymap string))
        (should (get-text-property label-beg 'face string))))))

(ert-deftest leman-room--toggle-spoiler-at-point ()
  "Test that toggling reveals and hides spoiler content.
Toggling works both from within the content and from its label."
  (let ((string (let ((leman-room-use-variable-pitch nil))
                  (leman-room--render-html
                   "<span data-mx-spoiler>hidden words</span>" nil))))
    (with-temp-buffer
      (insert string)
      ;; From within the content.
      (goto-char (next-single-property-change (point-min) 'leman-spoiler-content))
      (should (get-text-property (point) 'invisible))
      (leman-room--toggle-spoiler-at-point)
      (should-not (get-text-property (point) 'invisible))
      (leman-room--toggle-spoiler-at-point)
      (should (eq (get-text-property (point) 'invisible) 'leman-spoiler))
      ;; From the label.
      (goto-char (point-min))
      (search-forward "[spoiler]")
      (leman-room--toggle-spoiler-at-point)
      (should-not (get-text-property (point) 'invisible)))))

(provide 'leman-tests)

;;; leman-tests.el ends here
