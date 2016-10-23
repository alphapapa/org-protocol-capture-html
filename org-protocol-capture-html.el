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
(require 'cl)
(require 'subr-x)

;;;; Vars

(defcustom org-protocol-capture-html-demote-times 1
  "How many times to demote headings in captured pages.
You may want to increase this if you use a sub-heading in your capture template."
  :group 'org-protocol-capture-html :type 'integer)

;;;; Test Pandoc

(defconst org-protocol-capture-html-pandoc-no-wrap-option nil
  ;; Set this so it won't be unbound
  "Option to pass to Pandoc to disable wrapping.  Pandoc >= 1.16
deprecates `--no-wrap' in favor of `--wrap=none'.")

(defun org-protocol-capture-html-define-pandoc-wrap-const ()
  "Set `org-protocol-capture-html-pandoc-no-wrap-option'."
  (setq org-protocol-capture-html-pandoc-no-wrap-option
        ;; Pandoc >= 1.16 deprecates the --no-wrap option, replacing it with
        ;; --wrap=none.  Sending the wrong option causes output to STDERR,
        ;; which `call-process-region' doesn't like.  So we test Pandoc to see
        ;; which option to use.
        (with-temp-buffer
          (let* ((process (start-process "test-pandoc" (current-buffer) "pandoc" "--dump-args" "--no-wrap"))
                 (limit 3)
                 (checked 0))
            (while (process-live-p process)
              (if (= checked limit)
                  (progn
                    ;; Pandoc didn't exit in time.  Kill it and raise
                    ;; an error.  This function will return `nil' and
                    ;; `org-protocol-capture-html-pandoc-no-wrap-option'
                    ;; will remain `nil', which will cause this
                    ;; function to run again and set the const when a
                    ;; capture is run.
                    (set-process-query-on-exit-flag process nil)
                    (error "Unable to test Pandoc!  Please report this bug! (include the output of \"pandoc --dump-args --no-wrap\")"))
                (sleep-for 0.2)
                (cl-incf checked)))
            (if (and (= 0 (process-exit-status process))
                     (not (string-match "--no-wrap is deprecated" (buffer-string))))
                "--no-wrap"
              "--wrap=none")))))

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

  (unless org-protocol-capture-html-pandoc-no-wrap-option
    (org-protocol-capture-html-define-pandoc-wrap-const))

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
                     "pandoc" t t nil "-f" "html" "-t" "org" org-protocol-capture-html-pandoc-no-wrap-option)))
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

  (unless org-protocol-capture-html-pandoc-no-wrap-option
    (org-protocol-capture-html-define-pandoc-wrap-const))

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

      (unless (= 0 (call-process-region (point-min) (point-max) "pandoc" t t nil
                                        "-f" "html" "-t" "org" org-protocol-capture-html-pandoc-no-wrap-option))
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
      (dotimes (n org-protocol-capture-html-demote-times)
        (org-demote-subtree)))))

(provide 'org-protocol-capture-html)

;;; org-protocol-capture-html ends here
