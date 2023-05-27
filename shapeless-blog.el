;;; shapeless-blog.el --- Emacs interface for shapeless-blog -*- lexical-binding: t -*-

;;; Commentary:

;; A personal blogging tool in Emacs.

;; shapeless-blog always works with UTC timezone. Now the backend is
;; in pure Rust.

;; Version: 5.0.0

;;; Code:

(require 'cl-lib)
(require 'request)

(defcustom shapeless-blog-api-url "http://localhost:3000/api"
  "Api url to shapeless-blog server."
  :type 'string)

(defcustom shapeless-blog-token ""
  "The bearer token for authentication."
  :type 'string)

(defcustom shapeless-blog-username "jacky"
  "The username for shapeless blog."
  :type 'string)

(defcustom shapeless-blog-password "password"
  "The secret password to authenticate."
  :type 'string)

(defcustom shapeless-blog-token-expiry ""
  "The expiry date of token."
  :type 'string)

(defcustom shapeless-blog-export-backend 'shapelesshtml
  "The export backend of blog content."
  :type 'string)

(defun shapeless-blog-update-token ()
  "Update shapeless-blog token."
  (interactive)
  (request (concat shapeless-blog-api-url "/authentication")
    :type "POST"
    :data (json-encode (list `("username" . ,shapeless-blog-username)
                             `("password" . ,shapeless-blog-password)))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 201)
                     (let ((body (request-response-data response)))
                       (setq shapeless-blog-token (cdr (assoc 'token body)))
                       (setq shapeless-blog-token-expiry (cdr (assoc 'expired_time body)))
                       (message "updated token"))
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog--get-tags ()
  "Get tags for the current post.

Return an alist of tag string."
  (split-string (cadr (assoc "FILETAGS"
                             (org-collect-keywords '("filetags"))))
                ":" 'omit-nulls))

(defun shapeless-blog--get-title ()
  "Get title for the current post.

Return a string."
  (cadr (assoc "TITLE"
               (org-collect-keywords '("title")))))

(defun shapeless-blog--get-create-date ()
  "Get create date for the current post.

Return a string."
  (replace-regexp-in-string "[]\[<>]" ""
                            (cadr (assoc "DATE"
                                         (org-collect-keywords '("date"))))))

(defun shapeless-blog--get-update-date ()
  "Get update date for the current post.

Return a string."
  (replace-regexp-in-string "[]\[<>]" ""
                            (cadr (assoc "UPDATE"
                                         (org-collect-keywords '("update"))))))

(defun shapeless-blog--get-file-url ()
  "Get file url for the current post.

Return a string.

This is for the url field in shapeless-blog database."
  (shapeless-blog--title-to-file-url (shapeless-blog--get-title)))

(defun shapeless-blog--get-id ()
  "Get id of the current post.

Return an integer."
  (let ((id-string (cadr (assoc "ID"
                                (org-collect-keywords '("id"))))))
    (if (string-empty-p id-string)
        nil
      (string-to-number id-string))))

(defun shapeless-blog--edit-id (ID)
  "Edit the #+id: into ID.

ID is a integer."
  (replace-regexp "#\\+id:.*" (concat "#+id: " (number-to-string ID))
                  nil (point-min) (point-max)))

(defun shapeless-blog--edit-update-date (DATE)
  "Edit the #+update: into DATE.

DATE is a string."
  (replace-regexp "#\\+update:.*" (format "#+update: [%s]" DATE)
                  nil (point-min) (point-max)))

(defun shapeless-blog--edit-create-date (DATE)
  "Edit the #+date: into DATE.

DATE is a string."
  (replace-regexp "#\\+date:.*" (format "#+date: [%s]" DATE)
                  nil (point-min) (point-max)))

(defun shapeless-blog--title-to-file-url (TITLE)
  "Convert title into file url."
  (s-replace " " "-" (s-downcase TITLE)))

(defun shapeless-blog--get-preview ()
  "Return a string of the preview of the current blog."
  (save-excursion
    (goto-char (point-min))
    (let ((preview-start (- (re-search-forward "^[^[:punct:]\n]") 1))
          (preview-end   (- (re-search-forward "^*") 3)))
      (buffer-substring-no-properties preview-start preview-end))))

(defun shapeless-blog--get-current-buffer-html-body ()
  "Return a string of exported html of current buffer.

Using `shapeless-blog-export-backend'"
  (org-export-as shapeless-blog-export-backend nil nil t))

(defun shapeless-blog--get-current-buffer-json ()
  "Return a json format of the current blog post.

This json is for communicating with the shapeless-blog api."
  (json-encode
           (list (cons "title" (shapeless-blog--get-title))
                 (cons "url" (shapeless-blog--get-file-url))
                 (cons "preview" (shapeless-blog--get-preview))
                 (cons "tags" (shapeless-blog--get-tags))
                 (cons "content" (shapeless-blog--get-current-buffer-html-body))
                 (cons "create_time" (shapeless-blog--get-create-date))
                 (cons "edit_time" (shapeless-blog--get-update-date)))))

(defun shapeless-blog-create-post ()
  "Create a new post to server"
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blog/")
    :type "POST"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :parser 'json-read
    :data (shapeless-blog--get-current-buffer-json)
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 201)
                     (progn
                       (shapeless-blog--edit-id
                        (cdr (assoc 'id
                                    (request-response-data response))))
                       (shapeless-blog--edit-create-date
                        (format-time-string
                         "%Y-%m-%d"
                         (date-to-time
                          (cdr (assoc 'create_time
                                      (request-response-data response))))))
                       (shapeless-blog--edit-update-date
                        (format-time-string
                         "%Y-%m-%d"
                         (date-to-time
                          (cdr (assoc 'edit_time
                                    (request-response-data response))))))
                       (message "created post"))
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-update-post ()
  "Update the post of current buffer."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blog/"
                   (format "%d" (shapeless-blog--get-id)))
    :type "PATCH"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :data (shapeless-blog--get-current-buffer-json)
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 204)
                     (progn
                       (shapeless-blog--edit-update-date
                        (format-time-string "%Y-%m-%d"))
                       (message "updated post"))
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-force-create-post ()
  "Create a new post to the server.

Same as `shapeless-blog-create-post', but with custom create_time
and edit_time, useful when uploading old posts without updating
the time."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/force-blog/")
    :type "POST"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :parser 'json-read
    :data (shapeless-blog--get-current-buffer-json)
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 201)
                     (progn
                       (shapeless-blog--edit-id
                        (cdr (assoc 'id
                                    (request-response-data response))))
                       (message "force created post"))
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-force-update-post ()
  "Update the post of the current buffer.

Same with `shapeless-blog-update-post', but with custom create
and update time, useful when working with old blogs."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/force-blog/"
                   (format "%d" (shapeless-blog--get-id)))
    :type "PUT"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :data (shapeless-blog--get-current-buffer-json)
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 204)
                     (message "force updated post")
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-create-or-update-post ()
  "Create or update the current buffer as a blog post.

If id is nil, call `shapeless-blog-create-post'. Otherwise call
`shapeless-blog-update-post'.

This function will also change the date to current time."
  (interactive)
  (if (eq (shapeless-blog--get-id) 0)
      (shapeless-blog-create-post)
    (shapeless-blog-update-post))
  (save-buffer))

(defun shapeless-blog-show-post-with-id ()
  "Show the post of input id."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blog/"
                   (read-string "ID: "))
    :type "GET"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 200)
                     (message "%s" (request-response-data response))
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-delete-post ()
  "Delete a post of input id."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blog/"
                   (read-string "ID: "))
    :type "DELETE"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 204)
                     (message "post deleted")
                   (error "failed to delete post"))))))

(provide 'shapeless-blog)
;;; shapeless-blog.el ends here
