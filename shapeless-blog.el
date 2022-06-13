;;; shapeless-blog-new.el --- Emacs interface for shapeless-blog -*- lexical-binding: t -*-

;;; Commentary:

;; A personal blogging tool in Emacs.

;;; Code:

(require 'cl-lib)
(require 'request)

(defcustom shapeless-blog-api-url "http://localhost:9398/api"
  "Api url to shapeless-blog server."
  :type 'string)

(defcustom shapeless-blog-token ""
  "The bearer token for authentication."
  :type 'string)

(defcustom shapeless-blog-secret "testsecret"
  "The secret password to authenticate."
  :type 'string)

(defcustom shapeless-blog-token-expiry ""
  "The expiry date of token."
  :type 'string)

(defcustom shapeless-blog-export-backend 'shapelesshtml
  "The export backend of blog content."
  :type 'string)

(defun shapeless-blog-server-healthcheck ()
  "Check api server status."
  (interactive)
  (request (concat shapeless-blog-api-url "/healthcheck")
    :type "GET"
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (eq (request-response-status-code response) 200)
                     (message "%s" (request-response-data response))
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-update-token ()
  "Update shapeless-blog token."
  (interactive)
  (request (concat shapeless-blog-api-url "/tokens/authentication")
    :type "POST"
    :data (json-encode (list `("secret" . ,shapeless-blog-secret)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 201)
                     (let ((body (request-response-data response)))
                       (setq shapeless-blog-token (cdr (assoc 'token body)))
                       (setq shapeless-blog-token-expiry (cdr (assoc 'expiry body)))
                       (message "updated token"))
                   (error "%s" (request-response-data response)))
                 ))))

;; There are three templates available, "post", "home", "tag".
(defun shapeless-blog-create-template ()
  "Send a new template to server."
  (interactive)
  (request (concat shapeless-blog-api-url "/blogging/templates")
    :type "POST"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :data (json-encode (list `("name" . ,(file-name-sans-extension (buffer-name)))
                             `("content" . ,(buffer-substring-no-properties
                                             (point-min) (point-max)))))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 201)
                     (message "%s" (request-response-data response))
                   (error "%s" (request-response-data response))
                   )))))

(defun shapeless-blog-update-template ()
  "Update a template to server."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blogging/templates/"
                   (file-name-sans-extension (buffer-name)))
    :type "PUT"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :data (json-encode (list (cons "content"
                                   (buffer-substring-no-properties
                                    (point-min)
                                    (point-max)))))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 200)
                     (message "updated template")
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-show-template ()
  "Show a template of input name."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blogging/templates/"
                   (read-string "Template name: "))
    :type "GET"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 200)
                     (message "%s" (request-response-data response))
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-delete-template ()
  "Delete a template of input name."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blogging/templates/"
                   (read-string "Template name: "))
    :type "DELETE"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 204)
                     (message "the template is deleted")
                   (error "failed to delete template"))))))

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

(defun shapeless-blog--get-current-buffer-html-body ()
  "Return a string of exported html of current buffer.

Using `shapeless-blog-export-backend'"
  (org-export-as shapeless-blog-export-backend nil nil t))

(defun shapeless-blog-create-post ()
  "Create a new post to server"
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blogging/posts")
    :type "POST"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :parser 'json-read
    :data (json-encode
           (list (cons "title" (shapeless-blog--get-title))
                 (cons "url" (shapeless-blog--get-file-url))
                 (cons "tags" (shapeless-blog--get-tags))
                 (cons "content" (shapeless-blog--get-current-buffer-html-body))
                 (cons "create_at" (shapeless-blog--get-create-date))
                 (cons "update_at" (shapeless-blog--get-update-date))))
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 201)
                     (progn
                       (shapeless-blog--edit-id
                        (cdr (assoc 'id
                                    (request-response-data response))))
                       (message "created post"))
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-update-post ()
  "Update the post of current buffer."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blogging/posts/id/"
                   (format "%d" (shapeless-blog--get-id)))
    :type "PUT"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :data (json-encode
           (list (cons "title" (shapeless-blog--get-title))
                 (cons "url" (shapeless-blog--get-file-url))
                 (cons "tags" (shapeless-blog--get-tags))
                 (cons "content" (shapeless-blog--get-current-buffer-html-body))
                 (cons "create_at" (shapeless-blog--get-create-date))
                 (cons "update_at" (shapeless-blog--get-update-date))))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 200)
                     (message "updated post")
                   (error "%s" (request-response-data response)))))))

(defun shapeless-blog-create-or-update-post ()
  "Create or update the current buffer as a blog post.

If id is nil, call `shapeless-blog-create-post'. Otherwise call
`shapeless-blog-update-post'.

This function will also change the date to now."
  (interactive)
  (if (eq (shapeless-blog--get-id) 0)
      (progn
        (shapeless-blog--edit-create-date (format-time-string "%Y-%m-%d"))
        (shapeless-blog--edit-update-date (format-time-string "%Y-%m-%d"))
        (shapeless-blog-create-post))
    (progn
      (shapeless-blog--edit-update-date (format-time-string "%Y-%m-%d"))
      (shapeless-blog-update-post))))

(defun shapeless-blog-show-post-with-id ()
  "Show the post of input id."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blogging/posts/id/"
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

(defun shapeless-blog-show-post-with-url ()
  "Show the post of input id."
  (interactive)
  (request (concat shapeless-blog-api-url
                   "/blogging/posts/"
                   (read-string "URL: "))
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
                   "/blogging/posts/id/"
                   (read-string "ID: "))
    :type "DELETE"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " shapeless-blog-token)))
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (if (= (request-response-status-code response) 204)
                     (message "post deleted")
                   (error "failed to delete post"))))))

(provide 'shapeless-blog-new)
;;; shapeless-blog-new.el ends here
