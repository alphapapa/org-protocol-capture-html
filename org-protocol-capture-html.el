;;; org-protocol-capture-html --- Capture HTML with org-protocol

;;; Commentary:
;; This makes it possible to capture HTML into Org-mode with
;; org-protocol by passing it through Pandoc to convert the HTML into
;; Org syntax. You can use a JavaScript bookmarklet to get the HTML
;; from the browser's selection, such as this:
;;
;; content.location.href = 'org-protocol://capture-html://w/' + encodeURIComponent(content.location.href) + '/' + encodeURIComponent(content.document.title) + '/' + encodeURIComponent(function () {var html = ""; if (typeof content.document.getSelection != "undefined") {var sel = content.document.getSelection(); if (sel.rangeCount) {var container = document.createElement("div"); for (var i = 0, len = sel.rangeCount; i < len; ++i) {container.appendChild(sel.getRangeAt(i).cloneContents());} html = container.innerHTML;}} else if (typeof document.selection != "undefined") {if (document.selection.type == "Text") {html = document.selection.createRange().htmlText;}} var relToAbs = function (href) {var a = content.document.createElement("a"); a.href = href; var abs = a.protocol + "//" + a.host + a.pathname + a.search + a.hash; a.remove(); return abs;}; var elementTypes = [['a', 'href'], ['img', 'src']]; var div = content.document.createElement('div'); div.innerHTML = html; elementTypes.map(function(elementType) {var elements = div.getElementsByTagName(elementType[0]); for (var i = 0; i < elements.length; i++) {elements[i].setAttribute(elementType[1], relToAbs(elements[i].getAttribute(elementType[1])));}}); return div.innerHTML;}());

;;; Code:
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
	 (title (or (cadr parts) ""))
	 (content (or (caddr parts) ""))
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
                                :link url
                                :description title
                                :orglink orglink
                                :initial (buffer-string)))
        (raise-frame)
        (funcall 'org-capture nil template)

        ;; Demote page headings in capture buffer to below the
        ;; top-level Org heading
        (save-excursion
          (goto-char (point-min))
          (re-search-forward (rx bol "*" (1+ space)) nil t) ; Skip 1st heading
          (while (re-search-forward (rx bol "*" (1+ space)) nil t)
            (org-demote-subtree)))))
    nil))

(add-to-list 'org-protocol-protocol-alist
             '("capture-html"
               :protocol "capture-html"
               :function org-protocol-capture-html-with-pandoc
               :kill-client t))

(provide 'org-protocol-capture-html)
;;; org-protocol-capture-html ends here
