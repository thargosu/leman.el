;;;; leman-e2ee-tests.el --- Tests for leman-e2ee.el        -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the E2EE agent protocol layer.  Pure-protocol tests use
;; a fake in-memory transport; tests tagged `e2ee-real' drive the
;; real agent binary (skipped when it hasn't been built).

;;; Code:

(require 'ert)
(require 'json)

(require 'leman-e2ee)

;;;; Helpers

(defconst leman-e2ee-tests--root
  ;; Captured at load time: `load-file-name' is only bound then.
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Repository root directory.")

(defun leman-e2ee-tests--agent-program ()
  "Return the path to a built agent binary, or nil."
  (seq-find #'file-executable-p
            (list (expand-file-name "e2ee/agent/target/debug/leman-agent" leman-e2ee-tests--root)
                  (expand-file-name "e2ee/agent/target/release/leman-agent" leman-e2ee-tests--root))))

(defun leman-e2ee-tests--fake-agent (responses)
  "Return an agent whose fake transport sends RESPONSES.
RESPONSES is a list of lines delivered in response to any
request; nil responds nothing."
  (leman-e2ee--create
   :pending (make-hash-table :test #'eql)
   :fake (lambda (agent _line)
           (dolist (response responses)
             (leman-e2ee-handle-line agent response)))))

;;;; Encoding/decoding

(ert-deftest leman-e2ee-encode ()
  (should (equal (leman-e2ee--decode (leman-e2ee--encode 5 "hello" nil))
                 '((id . 5) (cmd . "hello"))))
  (should (equal (leman-e2ee--decode
                  (leman-e2ee--encode 6 "update_tracked_users"
                                      (list (cons 'users (vector "@a:x.org")))))
                 '((id . 6) (cmd . "update_tracked_users")
                   (params . ((users . ["@a:x.org"])))))))

(ert-deftest leman-e2ee-decode-invalid ()
  (should (null (leman-e2ee--decode "not json"))))

;;;; Response handling

(ert-deftest leman-e2ee-handle-line-stores-responses ()
  (let ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql))))
    (leman-e2ee-handle-line agent "{\"id\":1,\"ok\":{\"x\":2}}")
    (should (equal (gethash 1 (leman-e2ee-pending agent))
                   '(ok . ((x . 2)))))
    (leman-e2ee-handle-line agent "{\"id\":2,\"err\":{\"code\":\"parse\",\"message\":\"nope\"}}")
    (should (equal (gethash 2 (leman-e2ee-pending agent))
                   '(err . ((code . "parse") (message . "nope")))))))

(ert-deftest leman-e2ee-handle-line-ignores-stray-and-garbage ()
  (let ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql))))
    (leman-e2ee-handle-line agent "complete garbage")
    (leman-e2ee-handle-line agent "{\"err\":{\"code\":\"parse\"}}")
    (should (zerop (hash-table-count (leman-e2ee-pending agent))))))

;;;; Requests with the fake transport

(ert-deftest leman-e2ee-request-correlates-by-id ()
  ;; A stray response with a different id must not satisfy the request.
  (let* ((agent (leman-e2ee-tests--fake-agent
                 '("{\"id\":99,\"ok\":{\"stray\":true}}"
                   "{\"id\":1,\"ok\":{\"value\":42}}"))))
    (should (equal (alist-get 'value (leman-e2ee-request agent "hello"))
                   42))))

(ert-deftest leman-e2ee-request-signals-error ()
  (let ((agent (leman-e2ee-tests--fake-agent
                '("{\"id\":1,\"err\":{\"code\":\"crypto\",\"message\":\"boom\"}}"))))
    (should-error (leman-e2ee-request agent "decrypt_room_event")
                  :type 'leman-e2ee-error)))

(ert-deftest leman-e2ee-request-times-out ()
  (let ((leman-e2ee-request-timeout 0.1)
        (agent (leman-e2ee-tests--fake-agent nil)))
    (should-error (leman-e2ee-request agent "hello")
                  :type 'leman-e2ee-error)))

(ert-deftest leman-e2ee-request-no-transport ()
  (let ((agent (leman-e2ee--create)))
    (should-error (leman-e2ee-request agent "hello")
                  :type 'leman-e2ee-error)))

;;;; Real agent (requires a built binary)

(ert-deftest leman-e2ee-real-start-initialize-stop ()
  :tags '(e2ee-real)
  (skip-unless (leman-e2ee-tests--agent-program))
  (let* ((leman-e2ee-agent-program (leman-e2ee-tests--agent-program))
         (store (make-temp-file "leman-e2ee-test-" t))
         (agent (leman-e2ee-start "@bob:example.org" "TESTDEVICE" store)))
    (unwind-protect
        (let ((identity-keys (leman-e2ee-identity-keys agent)))
          (should (stringp (alist-get 'curve25519 identity-keys)))
          (should (stringp (alist-get 'ed25519 identity-keys))))
      (leman-e2ee-stop agent))))

(ert-deftest leman-e2ee-real-persistence ()
  :tags '(e2ee-real)
  (skip-unless (leman-e2ee-tests--agent-program))
  (let* ((leman-e2ee-agent-program (leman-e2ee-tests--agent-program))
         (store (make-temp-file "leman-e2ee-test-" t))
         (first (leman-e2ee-start "@bob:example.org" "TESTDEVICE" store))
         (identity-keys (leman-e2ee-identity-keys first)))
    (leman-e2ee-stop first)
    (let* ((second (leman-e2ee-start "@bob:example.org" "TESTDEVICE" store))
           (identity-keys-2 (leman-e2ee-identity-keys second)))
      (unwind-protect
          (should (equal identity-keys identity-keys-2))
        (leman-e2ee-stop second)))))

(ert-deftest leman-e2ee-real-outgoing-requests ()
  :tags '(e2ee-real)
  (skip-unless (leman-e2ee-tests--agent-program))
  (let* ((leman-e2ee-agent-program (leman-e2ee-tests--agent-program))
         (store (make-temp-file "leman-e2ee-test-" t))
         (agent (leman-e2ee-start "@bob:example.org" "TESTDEVICE" store)))
    (unwind-protect
        (let ((requests (leman-e2ee-outgoing-requests agent)))
          (should requests)
          (let ((request (elt requests 0)))
            (should (member (alist-get 'method request) '("POST" "PUT")))
            (should (string-prefix-p "/_matrix/client/"
                                     (alist-get 'path request)))
            (should (alist-get 'body request))))
      (leman-e2ee-stop agent))))

;;;; Footer

(provide 'leman-e2ee-tests)

;;;; leman-e2ee-tests.el ends here
