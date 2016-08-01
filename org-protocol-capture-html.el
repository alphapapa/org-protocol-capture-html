;;; org-protocol-capture-html.el --- Capture HTML with org-protocol

;;; Commentary:

;; This package captures Web pages into Org-mode using Pandoc to
;; process HTML.  It can also use python-readability to get article
;; content.

;; These are the helper functions that run in Emacs.  To capture pages
;; into Emacs, you can use either a browser bookmarklet or the
;; org-protocol-capture-html.sh shell script.  See the README.org file
;; for instructions.

;;; Code:

;;;; Require

(require 'org-protocol)
(require 'subr-x)

;;;; Direct-to-Pandoc

(defun org-protocol-capture-html-with-pandoc (data)
  "Process an org-protocol://capture-html:// URL.

This function is basically a copy of `org-protocol-do-capture', but
it passes the captured content (not the URL or title) through
Pandoc, converting HTML to Org-mode."

  ;; It would be nice to not basically duplicate
  ;; `org-protocol-do-capture', but passing the data back to that
  ;; function would require re-encoding the data into a URL string
  ;; with Emacs after Pandoc converts it.  Since we've already split
  ;; it up, we might as well go ahead and run the capture directly.

  (let* ((parts (org-protocol-split-data data t org-protocol-data-separator))
	 (template (or (and (>= 2 (length (car parts))) (pop parts))
		       org-protocol-default-template-key))
	 (url (org-protocol-sanitize-uri (car parts)))
	 (type (if (string-match "^\\([a-z]+\\):" url)
		   (match-string 1 url)))
	 (title (or (string-trim (cadr parts)) ""))
	 (content (or (string-trim (caddr parts)) ""))
	 (orglink (org-make-link-string
		   url (if (string-match "[^[:space:]]" title) title url)))
	 (query (or (org-protocol-convert-query-to-plist (cadddr parts)) ""))
	 (org-capture-link-is-already-stored t)) ; avoid call to org-store-link

    (setq org-stored-links
          (cons (list url title) org-stored-links))
    (kill-new orglink)

    (with-temp-buffer
      (insert content)
      (if (not (= 0 (call-process-region
                     (point-min) (point-max)
                     "pandoc" t t nil "--no-wrap" "-f" "html" "-t" "org")))
          (message "Pandoc failed: " (buffer-string))
        (progn
          ;; Pandoc succeeded
          (org-store-link-props :type type
                                :annotation orglink
                                :link url
                                :description title
                                :orglink orglink
                                :initial (buffer-string)))))
    (org-protocol-capture-html-do-capture)
    nil))

(add-to-list 'org-protocol-protocol-alist
             '("capture-html"
               :protocol "capture-html"
               :function org-protocol-capture-html-with-pandoc
               :kill-client t))

;;;; Readability

(defun org-protocol-capture-readability (data)
  "Capture content of URL with readability-lxml Python package."

  (let* ((parts (org-protocol-split-data data t org-protocol-data-separator))
	 (template (or (and (>= 2 (length (car parts))) (pop parts))
		       org-protocol-default-template-key))
	 (url (org-protocol-sanitize-uri (car parts)))
	 (type (if (string-match "^\\([a-z]+\\):" url)
		   (match-string 1 url)))
	 (title (or (string-trim (cadr parts)) ""))
	 (content (or (string-trim (caddr parts)) ""))
	 (orglink (org-make-link-string
		   url (if (string-match "[^[:space:]]" title) title url)))
	 (query (or (org-protocol-convert-query-to-plist (cadddr parts)) ""))
         ;; Avoid call to org-store-link
	 (org-capture-link-is-already-stored t))

    (setq org-stored-links
          (cons (list url title) org-stored-links))
    (kill-new orglink)

    (with-temp-buffer
      (unless (= 0 (call-process "python" nil '(t t) nil  "-m" "readability.readability" "-u" url))
        (error "Python readability-lxml script failed: %s" (buffer-string)))

      ;; Get title if necessary
      (goto-char (point-min))
      (if (not (string= title ""))
          (progn
            ;; Skip first line containing page title; we already have it
            (delete-region (point) (line-end-position)))
        ;; Get title
        (setq title (buffer-substring-no-properties (search-forward "Title:") (line-end-position)))
        (setq orglink (org-make-link-string url (if (string-match "[^[:space:]]" title) title url))))

      (unless (= 0 (call-process-region (point-min) (point-max) "pandoc" t t nil "--no-wrap" "-f" "html" "-t" "org"))
        (error "Pandoc failed."))

      (org-store-link-props :type type
                            :annotation orglink
                            :link url
                            :description title
                            :orglink orglink
                            :initial (buffer-string)))
    (org-protocol-capture-html-do-capture)
    nil))

(add-to-list 'org-protocol-protocol-alist
             '("capture-readability"
               :protocol "capture-readability"
               :function org-protocol-capture-readability
               :kill-client t))

;;;; Helper functions

(defun org-protocol-capture-html-do-capture ()
  "Call `org-capture' and demote page headings in capture buffer."
  (raise-frame)
  (funcall 'org-capture nil template)

  ;; Demote page headings in capture buffer to below the
  ;; top-level Org heading
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (rx bol "*" (1+ space)) nil t) ; Skip 1st heading
    (while (re-search-forward (rx bol "*" (1+ space)) nil t)
      (org-demote-subtree))))

(provide 'org-protocol-capture-html)

;;; org-protocol-capture-html ends here
