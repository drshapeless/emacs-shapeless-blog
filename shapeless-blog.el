;;; shapeless-blog.el --- Emacs interface for shapeless-blog -*- lexical-binding: t -*-

;;; Commentary:

;; A personal blogging tool in Emacs.

;;; Code:

(defcustom shapeless-blog-default-directory
  (concat (getenv "HOME") "/shapeless-blog/")
  "Default directory of shapeless-blog data."
  :type 'string)

(defcustom shapeless-blog-database-path
  (concat shapeless-blog-default-directory "shapeless-blog.db")
  "Path to shapeless-blog sqlite database."
  :type 'string)

(defcustom shapeless-blog-export-backend 'shapelesshtml
  "The export backend of blog content."
  :type 'string)

(defcustom shapeless-blog-remote-path ""
  "The remote path of shapeless-blog database."
  :type 'string)

(defun shapeless-blog-migrate (&optional PATH)
  "Migrate the sqlite database at PATH.

PATH is a string to the database location.

If PATH is nil, use `shapeless-blog-database-path' as default."
  (interactive)
  (let ((sql (sqlite-open (if (null PATH)
                              shapeless-blog-database-path
                            PATH))))
    (sqlite-execute
     sql
     "CREATE TABLE IF NOT EXISTS posts(
          id INTEGER PRIMARY KEY,
          title TEXT NOT NULL,
          filename TEXT NOT NULL,
          created TEXT NOT NULL,
          updated TEXT NOT NULL
      );"
     )
    (sqlite-execute
     sql
     "CREATE TABLE IF NOT EXISTS tags(
          post_id INTEGER NOT NULL,
          tag TEXT
      );"
     )))

(defun shapeless-blog-destroy-tables (&optional PATH)
  "Drop all the tables in the database at PATH.

PATH is a string to the database location.

If PATH is nil, use `shapeless-blog-database-path' as default."
  (interactive)
  (let ((sql (sqlite-open (if (null PATH)
                              shapeless-blog-database-path
                            PATH))))
    (sqlite-execute
     sql
     "DROP TABLE IF EXISTS posts;")
    (sqlite-execute
     sql
     "DROP TABLE IF EXISTS tags;")))

(defun shapeless-blog-reset-database ()
  "Reset database."
  (interactive)
  (shapeless-blog-destroy-tables)
  (shapeless-blog-migrate))

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

(defun shapeless-blog--get-filename ()
  "Get filename for the current post.

Return a string.

This does not mean the actual filename, but a filename for the
shapeless-blog database."
  (s-replace " " "_" (s-downcase (shapeless-blog--get-title))))

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

(defun shapeless-blog--title-to-filename (TITLE)
  "Convert title into filename."
  (s-replace " " "_" (s-downcase TITLE)))

(defun shapeless-blog--db-insert-tag1 (SQL ID TAG)
  (sqlite-execute SQL
                  "INSERT INTO tags(post_id, tag) VALUES (?, ?);"
                  (list ID TAG)))

(defun shapeless-blog--db-insert-tags (SQL ID TAGS)
  "Add tags of TAGS to post of ID."
  (mapcar (lambda (tag) (shapeless-blog--db-insert-tag1 SQL ID tag))
          TAGS))

(defun shapeless-blog--db-insert-post (SQL TITLE FILENAME CREATED UPDATED)
  "Insert a new post into database.

Returning the id."
  (caar (sqlite-select
         SQL
         "INSERT INTO posts(title, filename, created, updated) VALUES (?, ?, ?, ?) RETURNING id;"
         (list TITLE
               FILENAME
               CREATED
               UPDATED))))

(defun shapeless-blog--db-insert-old-post (SQL ID TITLE FILENAME CREATED UPDATED)
  "Insert an old post into database.

The difference between this and `shapeless-blog--db-insert-post'
is this function requires an explicit id number, while the other
one generate one."
  (sqlite-execute
   SQL
   "INSERT INTO posts(id, title, filename, created, updated) VALUES(?,?,?,?,?);"
   (list ID TITLE FILENAME CREATED UPDATED)))

(defun shapeless-blog--db-update-post (SQL ID TITLE FILENAME CREATED UPDATED)
  "Update an old post in the database."
  (sqlite-execute SQL
                  "UPDATE posts
SET title = ?, filename = ?, created = ?, updated = ?
WHERE id = ?"
                  (list TITLE FILENAME CREATED UPDATED ID)))

(defun shapeless-blog--db-delete-post (SQL ID)
  "Delete post of ID from the database."
  (sqlite-execute SQL
                  "DELETE FROM posts WHERE id = ?;"
                  (list ID)))

(defun shapeless-blog--db-delete-all-tags (SQL ID)
  "Delete tags with post_id ID from the database."
  (sqlite-execute SQL
                  "DELETE FROM tags WHERE post_id = ?;"
                  (list ID)))

(defun shapeless-blog--export-to-file (FILENAME)
  "Export the current buffer into FILENAME in the database."
  (org-export-to-file shapeless-blog-export-backend
      (concat shapeless-blog-default-directory
              (format "/posts/%s.html" FILENAME))
    t nil nil t))

(defun shapeless-blog--create-new-post ()
  "Create a new post."
  (let* ((sql (sqlite-open shapeless-blog-database-path))
         (now (format-time-string "%Y-%m-%d %H:%M"))
         (title (shapeless-blog--get-title))
         (filename (shapeless-blog--title-to-filename title))
         (tags (shapeless-blog--get-tags))
         (id (shapeless-blog--db-insert-post sql title filename now now)))
    (shapeless-blog--edit-id id)
    (shapeless-blog--edit-create-date now)
    (shapeless-blog--edit-update-date now)
    (shapeless-blog--db-insert-tags sql id tags)
    (shapeless-blog--export-to-file filename)
    ))

(defun shapeless-blog--update-old-post ()
  "Update an old post."
  (let* ((sql (sqlite-open shapeless-blog-database-path))
         (created (shapeless-blog--get-create-date))
         (now (format-time-string "%Y-%m-%d %H:%M"))
         (title (shapeless-blog--get-title))
         (filename (shapeless-blog--title-to-filename title))
         (id (shapeless-blog--get-id))
         (tags (shapeless-blog--get-tags)))
    (shapeless-blog--edit-update-date now)
    (shapeless-blog--db-delete-all-tags sql id)
    (shapeless-blog--db-update-post sql id title filename created now)
    (shapeless-blog--db-insert-tags sql id tags)
    ;; In some rare cases of editing the title will result in a
    ;; redundant html file in the database. Manual deletion is
    ;; required.
    (shapeless-blog--export-to-file filename)
    ))

(defun shapeless-blog-create-old-post ()
  "Create an old post."
  (interactive)
  (let* ((sql (sqlite-open shapeless-blog-database-path))
         (created (shapeless-blog--get-create-date))
         (updated (shapeless-blog--get-update-date))
         (title (shapeless-blog--get-title))
         (filename (shapeless-blog--title-to-filename title))
         (id (shapeless-blog--get-id))
         (tags (shapeless-blog--get-tags)))
    (shapeless-blog--db-delete-all-tags sql id)
    (shapeless-blog--db-insert-old-post sql id title filename created updated)
    (shapeless-blog--db-insert-tags sql id tags)
    ;; In some rare cases of editing the title will result in a
    ;; redundant html file in the database. Manual deletion is
    ;; required.
    (shapeless-blog--export-to-file filename)
    ))

(defun shapeless-blog-modify-old-post ()
  "Modify an old post without changing the updated time."
  (interactive)
  (let* ((sql (sqlite-open shapeless-blog-database-path))
         (created (shapeless-blog--get-create-date))
         (updated (shapeless-blog--get-update-date))
         (title (shapeless-blog--get-title))
         (filename (shapeless-blog--title-to-filename title))
         (id (shapeless-blog--get-id))
         (tags (shapeless-blog--get-tags)))
    (shapeless-blog--db-delete-all-tags sql id)
    (shapeless-blog--db-update-post sql id title filename created updated)
    (shapeless-blog--db-insert-tags sql id tags)
    ;; In some rare cases of editing the title will result in a
    ;; redundant html file in the database. Manual deletion is
    ;; required.
    (shapeless-blog--export-to-file filename)
    ))

(defun shapeless-blog-create-or-update-post ()
  "Create or update the current buffer.

If #+id: is empty, create a new post, otherwise update the post."
  (interactive)
  (if (null (shapeless-blog--get-id))
      (shapeless-blog--create-new-post)
    (shapeless-blog--update-old-post)))

(defun shapeless-blog-sync ()
  "Sync with the remote path using 'rsync'."
  (interactive)
  (async-shell-command (format "rsync -urv --delete-after %s %s"
                               shapeless-blog-default-directory
                               shapeless-blog-remote-path)
                       "*shapeless-blog-rsync*"))

(provide 'shapeless-blog)
;;; shapeless-blog.el ends here
