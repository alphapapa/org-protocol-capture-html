;;; org-protocol-capture-html.el --- Capture HTML with org-protocol

;; URL: https://github.com/alphapapa/org-protocol-capture-html
;; Version: 0.1-pre
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package captures Web pages into Org-mode using Pandoc to
;; process HTML.  It can also use eww's eww-readable functionality to
;; get the main content of a page.

;; These are the helper functions that run in Emacs.  To capture pages
;; into Emacs, you can use either a browser bookmarklet or the
;; org-protocol-capture-html.sh shell script.  See the README.org file
;; for instructions.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Require

(require 'org-protocol)
(require 'cl-lib)
(require 'subr-x)
(require 's)

;;;; Vars

(defcustom org-protocol-capture-html-demote-times 1
  "How many times to demote headings in captured pages.
You may want to increase this if you use a sub-heading in your capture template."
  :group 'org-protocol-capture-html :type 'integer)

;;;; Test Pandoc

(defconst org-protocol-capture-html-pandoc-no-wrap-option nil
  ;; Set this so it won't be unbound
  "Option to pass to Pandoc to disable wrapping.
Pandoc >= 1.16 deprecates `--no-wrap' in favor of
`--wrap=none'.")

(defun org-protocol-capture-html--define-pandoc-wrap-const ()
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
            (if (and (zerop (process-exit-status process))
                     (not (string-match "--no-wrap is deprecated" (buffer-string))))
                "--no-wrap"
              "--wrap=none")))))

;;;; Direct-to-Pandoc

(defun org-protocol-capture-html--with-pandoc (data)
  "Process an org-protocol://capture-html:// URL using DATA.

This function is basically a copy of `org-protocol-do-capture', but
it passes the captured content (not the URL or title) through
Pandoc, converting HTML to Org-mode."

  ;; It would be nice to not basically duplicate
  ;; `org-protocol-do-capture', but passing the data back to that
  ;; function would require re-encoding the data into a URL string
  ;; with Emacs after Pandoc converts it.  Since we've already split
  ;; it up, we might as well go ahead and run the capture directly.

  (unless org-protocol-capture-html-pandoc-no-wrap-option
    (org-protocol-capture-html--define-pandoc-wrap-const))

  (let* ((template (or (plist-get data :template)
                       org-protocol-default-template-key))
         (url (org-protocol-sanitize-uri (plist-get data :url)))
         (type (if (string-match "^\\([a-z]+\\):" url)
                   (match-string 1 url)))
         (title (or (org-protocol-capture-html--nbsp-to-space (string-trim (plist-get data :title))) ""))
         (content (or (org-protocol-capture-html--nbsp-to-space (string-trim (plist-get data :body))) ""))
         (orglink (org-make-link-string
                   url (if (string-match "[^[:space:]]" title) title url)))
         (org-capture-link-is-already-stored t)) ; avoid call to org-store-link

    (setq org-stored-links
          (cons (list url title) org-stored-links))
    (kill-new orglink)

    (with-temp-buffer
      (insert content)
      (if (not (zerop (call-process-region
                       (point-min) (point-max)
                       "pandoc" t t nil "-f" "html" "-t" "org" org-protocol-capture-html-pandoc-no-wrap-option)))
          (message "Pandoc failed: %s" (buffer-string))
        (progn
          ;; Pandoc succeeded
          (org-store-link-props :type type
                                :annotation orglink
                                :link url
                                :description title
                                :orglink orglink
                                :initial (buffer-string)))))
    (org-protocol-capture-html--do-capture)
    nil))

(add-to-list 'org-protocol-protocol-alist
             '("capture-html"
               :protocol "capture-html"
               :function org-protocol-capture-html--with-pandoc
               :kill-client t))

;;;; eww-readable

(defvar url-http-end-of-headers)

(eval-when-compile
  ;; eww-readable only works on Emacs >=25.1, but I think it's better
  ;; to check for the actual symbols.  I think using
  ;; `eval-when-compile' is the right way to do this, but I'm not
  ;; sure.
  (when (and (require 'eww nil t)
             (require 'dom nil t)
             (fboundp 'eww-score-readability))

    (defun org-protocol-capture-html--capture-eww-readable (data)
      "Capture content of URL with eww-readable.."

      (unless org-protocol-capture-html-pandoc-no-wrap-option
        (org-protocol-capture-html--define-pandoc-wrap-const))

      (let* ((template (or (plist-get data :template)
                           org-protocol-default-template-key))
             (url (org-protocol-sanitize-uri (plist-get data :url)))
             (type (if (string-match "^\\([a-z]+\\):" url)
                       (match-string 1 url)))
             (html (org-protocol-capture-html--url-html url))
             (result (org-protocol-capture-html--eww-readable html))
             (title (cdr result))
             (content (with-temp-buffer
                        (insert (org-protocol-capture-html--nbsp-to-space (car result)))
                        ;; Convert to Org with Pandoc
                        (unless (= 0 (call-process-region (point-min) (point-max)
                                                          "pandoc" t t nil "-f" "html" "-t" "org"
                                                          org-protocol-capture-html-pandoc-no-wrap-option))
                          (error "Pandoc failed"))
                        (save-excursion
                          ;; Remove DOS CR/LF line endings
                          (goto-char (point-min))
                          (while (search-forward (string ?\C-m) nil t)
                            (replace-match "")))
                        ;; Demote page headings in capture buffer to below the
                        ;; top-level Org heading and "Article" 2nd-level heading
                        (save-excursion
                          (goto-char (point-min))
                          (while (re-search-forward (rx bol (1+ "*") (1+ space)) nil t)
                            (beginning-of-line)
                            (insert "**")
                            (end-of-line)))
                        (buffer-string)))
             (orglink (org-make-link-string
                       url (if (s-present? title) title url)))
             ;; Avoid call to org-store-link
             (org-capture-link-is-already-stored t))

        (setq org-stored-links
              (cons (list url title) org-stored-links))
        (kill-new orglink)

        (org-store-link-props :type type
                              :annotation orglink
                              :link url
                              :description title
                              :orglink orglink
                              :initial content)
        (org-protocol-capture-html--do-capture)
        nil))

    (add-to-list 'org-protocol-protocol-alist
                 '("capture-eww-readable"
                   :protocol "capture-eww-readable"
                   :function org-protocol-capture-html--capture-eww-readable
                   :kill-client t))

    (defun org-protocol-capture-html--url-html (url)
      "Return HTML from URL as string."
      (let* ((response-buffer (url-retrieve-synchronously url nil t))
             (encoded-html (with-current-buffer response-buffer
                             (pop-to-buffer response-buffer)
                             ;; Skip HTTP headers, using marker provided by url-http
                             (delete-region (point-min) (1+ url-http-end-of-headers))
                             (buffer-string))))
        (kill-buffer response-buffer)     ; Not sure if necessary to avoid leaking buffer
        (with-temp-buffer
          ;; For some reason, running `decode-coding-region' in the
          ;; response buffer has no effect, so we have to do it in a
          ;; temp buffer.
          (insert encoded-html)
          (condition-case nil
              ;; Fix undecoded text
              (decode-coding-region (point-min) (point-max) 'utf-8)
            (coding-system-error nil))
          (buffer-string))))

    (defun org-protocol-capture-html--eww-readable (html)
      "Return `eww-readable' part of HTML with title.
Returns list (HTML . TITLE)."
      ;; Based on `eww-readable'
      (let* ((html
              ;; Convert "&nbsp;" in HTML to plain spaces.
              ;; `libxml-parse-html-region' turns them into
              ;; underlines.  The closest I can find to an explanation
              ;; is at <http://www.perlmonks.org/?node_id=825188>.
              (org-protocol-capture-html--nbsp-to-space html))
             (dom (with-temp-buffer
                    (insert html)
                    (libxml-parse-html-region (point-min) (point-max))))
             (title (cl-caddr (car (dom-by-tag dom 'title)))))
        (eww-score-readability dom)
        (cons (with-temp-buffer
                (shr-dom-print (eww-highest-readability dom))
                (buffer-string))
              title)))))

;;;; Helper functions

(defun org-protocol-capture-html--nbsp-to-space (s)
  "Convert HTML non-breaking spaces to plain spaces in S."
  ;; Not sure why sometimes these are in the HTML and Pandoc converts
  ;; them to underlines instead of spaces, but this fixes it.
  (replace-regexp-in-string (rx "&nbsp;") " " s t t))

(with-no-warnings
  ;; Ignore warning about the dynamically scoped `template' variable.
  (defun org-protocol-capture-html--do-capture ()
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
          (org-demote-subtree))))))

(provide 'org-protocol-capture-html)

;;; org-protocol-capture-html.el ends here
