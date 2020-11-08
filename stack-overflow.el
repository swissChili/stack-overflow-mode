;; stack-overflow.el -- search stack overflow from emacs
;; written by swissChili (swisschili.sh)

(provide 'stack-overflow)

(require 'url-util)
(require 'cl)

(defvar *stack-overflow-link-handler* 'eww)

(defun stack-overflow ()
  "Browse Stack Overflow"
  (interactive)
  (let ((query (read-string "stack overflow query ")))
    (switch-to-buffer "stack-overflow-search")
    (stack-overflow-mode)
    (stack-overflow-search query)))

(define-derived-mode stack-overflow-mode special-mode "stack-overflow")

(defun stack-overflow-api-request (method url)
  (let ((url-request-method "GET"))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (let ((body (buffer-string)))
        (kill-buffer (current-buffer))
        (json-read-from-string body)))))

(defun stack-overflow-search-url (query)
  (concat "http://api.stackexchange.com/2.2/search?site=stackoverflow&intitle="
          (url-hexify-string query)))

;; Is there a better way to do this than several regex passes?
(defun stack-overflow-html-unescape (html)
  (let* ((lt (replace-regexp-in-string "&lt;" "<" html))
         (amp (replace-regexp-in-string "&amp;" "&" lt))
         (quot (replace-regexp-in-string "&quot;" "\"" amp)))
    quot))

(defun stack-overflow-insert-question (item)
  (insert (propertize (format "%s\n" (stack-overflow-html-unescape (cdr (assoc 'title item))))
                      'face 'info-title-4))
  (insert (if (equal t (cdr (assoc 'is_answered item))) ; :json-false otherwise so t is necessary
              (propertize "Answered " 'face 'compilation-info)
              "Not answered "))
  (insert-button "View" 'action (lexical-let ((link (cdr (assoc 'link item))))
                                  (lambda (button)
                                    (funcall *stack-overflow-link-handler* link))))
  (insert "\n\n"))

(defun stack-overflow-search (query)
  "Search on stack overflow"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (concat "Stack Overflow results for " query "\n\n")
                        'face 'info-node))
    (let* ((json (stack-overflow-api-request "GET" (stack-overflow-search-url query)))
           (items (cdr (assoc 'items json))))
      (dotimes (i (length items))
        (let ((item (elt items i)))
          (stack-overflow-insert-question item))))
    (beginning-of-buffer)))
