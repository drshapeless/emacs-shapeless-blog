;;; shapeless-blog.el --- Minimalistic blogging with org mode. -*- lexical-binding: t -*-

;;; Commentary:

;; Shapeless Blog is a minimalistic blogging package in Emacs. It
;; works with a shapeless-blog server. Aim at providing a simple and
;; minimal blogging experience with Emacs.

;;; Code:

(require 'ox-shapelesshtml)
(require 'request)

;; Early version of shapeless-blog only support pre-generated tokens.

;; (defvar slblog-authentication-address nil
;;   "The address for slblog authentication.")

(defcustom slblog-api-address nil
  "The api address for blog posting."
  :type 'string)

;; (defvar slblog-user nil
;;   "The user name of slblog.")

;; (defvar slblog-password nil
;;   "The user password of slblog.")

(defcustom slblog-token nil
  "The authentication token of slblog.
nil by default."
  :type 'string)

;; (defvar slblog-expire-time nil
;;   "The expire date of token.")



;; From https://stackoverflow.com/questions/66574715/how-to-get-org-mode-file-title-and-other-file-level-properties-from-an-arbitra.
(defun slblog--org-get-value-of-key (DATA KEY)
  "Return the value associated with the KEY.
DATA is a string from `org-element-parse-buffer'
KEY is a string of the name of org element."
  (nth 1
       (assoc KEY
              (org-element-map DATA
                  '(keyword)
                (lambda (kwd)
                  (let ((x (cadr kwd)))
                    (list (plist-get x :key)
                          (plist-get x :value))))))))

(defun slblog--org-parse-greater-element-from-path (PATH)
  "Return a string containing org-data.
PATH is a string of file path."
  (with-temp-buffer
    (insert-file-contents PATH)
    (org-mode)
    (org-element-parse-buffer 'greater-element)))

(defun slblog--org-get-value-of-key-from-path (PATH KEY)
  "Return the value associated with the KEY.
PATH is a string of file path.
KEY is a string of the name of org element."
  (slblog--org-get-value-of-key (slblog--org-parse-greater-element-from-path PATH)
                                KEY))

(defun slblog--org-current-buffer-get-value-of-key (KEY)
  "Return the value of key from the current org buffer.
KEY is a string of a org element name.
e.g. TITLE, CATEGORY"
  (slblog--org-get-value-of-key (org-element-parse-buffer 'greater-element)
                                KEY))

(defun slblog--org-current-buffer-get-category ()
  (slblog--org-current-buffer-get-value-of-key "CATEGORY"))

(defun slblog--org-current-buffer-get-title ()
  (slblog--org-current-buffer-get-value-of-key "TITLE"))

(defun slblog--org-current-buffer-get-date ()
  (slblog--org-current-buffer-get-value-of-key "DATE"))

(defun slblog--org-current-buffer-get-id ()
  (slblog--org-current-buffer-get-value-of-key "ID"))

(defun slblog--html-body-content ()
  (org-export-as 'shapelesshtml nil nil t))

(defun slblog--remove-timestamp-bracket (TIMESTAMP)
  "Remove the brackets of TIMESTAMP.

Return a TIMESTAMP string without brackets.

TIMESTAMP is a string."
  (replace-regexp-in-string "[]\[<>]" TIMESTAMP))

(defun slblog--update-time (TIMESTAMP)
  "Update the #+UPDATE: in org to TIMESTAMP.

TIMESTAMP is a string."
  (replace-regexp "#\\+UPDATE:.*" (concat "#+UPDATE: " TIMESTAMP)
                  nil (point-min) (point-max)))

(defun slblog--update-id (ID)
  "Update the #+ID: in org to ID.

ID is a string."
  (replace-regexp "#\\+ID:.*" (concat "#+ID: " ID)
                  nil (point-min) (point-max)))

(defun slblog--current-buffer-json-string ()
  (json-encode (list (cons "title" (slblog--org-current-buffer-get-title))
                     (cons "created_at" (slblog--remove-timestamp-bracket
                                         (slblog--org-current-buffer-get-date)))
                     (cons "last_update" ())
                     (cons "category" (slblog--org-current-buffer-get-category))
                     (cons "body" (slblog--html-body-content))
                     (cons "token" slblog-token)
                     )))

(defun slblog--new-post-object (UPDATE)
  "Return a list of objects of the new post.

UPDATE is a timestamp string.
Note that it does not have an id."
  (list (cons "title" (slblog--org-current-buffer-get-title))
        (cons "created_at" (slblog--remove-timestamp-bracket
                            (slblog--org-current-buffer-get-date)))
        (cons "last_update" (slblog--remove-timestamp-bracket
                             UPDATE))
        (cons "category" (slblog--org-current-buffer-get-category))
        (cons "body" (slblog--html-body-content))
        (cons "token" slblog-token)))

(defun slblog-post ()
  "JSON-STRING is a string of request json."
  (let ((id (slblog--org-current-buffer-get-id)))
    (if (string= id "nil")
        (message "new post")
      (message "old post")))
  )

(defun slblog--new-post-request (UPDATE)
  "Make a POST request to the api server.

UPDATE is a timestamp string."
  (request-response-data
   (request slblog-api-address
     :sync t
     ;; Timeout is necessary.
     :timeout 2
     :type "POST"
     :parser 'json-read
     :data (json-encode (slblog--new-post-object UPDATE)))))

(defvar slblog--template-string
  "#+TITLE: %s
#+DATE: %s
#+CATEGORY: %s
#+ID: %d

%s")

;; (defun slblog--update-token (token)
;;   "Token is a string return by the server."
;;   (setq slblog-token token))

;; (defun slblog--update-expire (time)
;;   (setq slblog-expire-time time))

;; (defun slblog-authenticate ()
;;   (let ((data (request-response-data
;;                (request slblog-authentication-address
;;                  :sync t
;;                  :timeout 2
;;                  :parser 'json-read
;;                  :data (json-encode (list (cons "user"
;;                                                 slblog-user)
;;                                           (cons "password"
;;                                                 slblog-password)))))))
;;     (if (string= (caar data) 'error)
;;         (message "slblog authentication failed: %S" (cdar data))
;;       (progn
;;         (slblog--update-token (cdr (assoc 'token (car data))))))))

(provide 'shapeless-blog)
;;; shapeless-blog.el ends here
