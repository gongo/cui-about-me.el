;;; cui-about-me.el -- control for cui-about.me
;;
;; MAHALO License (based on MIT License)
;;
;; Copyright (c) 2012 Wataru MIYAGUNI (gonngo _at_ gmail.com)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; 1. The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 2. Shall be grateful for something (including, but not limited this software).
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Usage:
;;
;; Initialize:
;;
;;     (require 'cui-about-me)
;;
;; GET cui-about.me/users:
;;
;;     M-x cui-about:users
;;
;; PUT cui-about.me/user:
;;
;;    eval following like (example) .
;;
;;      (cui-about:update "username" "password"
;;                        '(("github" . "https://github.com/username")
;;                          ("twitter" . "@username")))
;;
;; POST cui-about.me/signup
;;
;;    no support. sorry...
;;

(eval-when-compile (require 'cl))
(require 'anything)
(require 'url-http)


(defconst cui-about:buffer-name "*cui-about.me*")
(defconst cui-about:uri "http://cui-about.me")

;;------------------------------
;; Define exception error
;;------------------------------
(put 'cui-about:exception-not-retrieved 'error-message
     "cui-about.me - Not retrieved")
(put 'cui-about:exception-not-retrieved 'error-conditions
     '(cui-about:exception-not-retrieved error))

(defun cui-about:http-get (uri &optional buffer)
  (if (null buffer) (setq buffer cui-about:buffer-name))
  (ignore-errors (kill-buffer cui-about:buffer-name))
  (unless (eq 0 (call-process "curl" nil `(,buffer nil) nil
                              "-f"
                              "-X" "GET"
                              uri))
    (signal 'cui-about:exception-not-retrieved
            "The requested URI returned error")))

(defun cui-about:http-put (uri args)
  (with-temp-buffer
    (apply 'call-process "curl" nil (current-buffer) nil
           "-s" "-S" "-f" "-X" "PUT" uri args)
    (message (replace-regexp-in-string "\n+$" "" (buffer-string)))))

(defun cui-about:update (user pass kvlist)
  (let ((params `("-d" ,(concat "password=" (url-hexify-string pass)))))
    (dolist (kv kvlist)
      (nconc params
             (list "-d" (concat (car kv) "=" (url-hexify-string (cdr kv))))))
    (cui-about:http-put (concat cui-about:uri "/" user) params)
    ))

(defun cui-about:init-users ()
  (with-current-buffer (anything-candidate-buffer 'local)
    (cui-about:http-get (concat cui-about:uri "/users")
                        (current-buffer))))

(defun cui-about:make-kv-list (user)
  (let ((kvlist '()))
    (with-temp-buffer
      (cui-about:http-get (concat cui-about:uri "/" user) (current-buffer))
      (beginning-of-buffer)
      (let (kv key value)
        (dotimes (i (count-lines (point-min) (point-max)))
          (setq kv (buffer-substring (point-at-bol) (point-at-eol)))
          (string-match "\\([^=]+\\) = \\(.+\\)" kv)
          (add-to-list 'kvlist (cons (match-string 1 kv) (match-string 2 kv)))
          (next-line))))
    kvlist))

(defun cui-about:show (user)
  (let ((kvlist (cui-about:make-kv-list user)))
    (with-current-buffer (get-buffer-create cui-about:buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (kv kvlist)
        (insert (concat "|" (car kv) "|" (cdr kv) "|\n")))
      (orgtbl-mode)
      (beginning-of-buffer)
      (org-table-recalculate)
      (beginning-of-buffer)
      (insert (concat cui-about:uri "/" user "\n\n"))
      (setq buffer-read-only t)
      (view-mode)))
  (switch-to-buffer-other-window cui-about:buffer-name))

(defun cui-about:users ()
  (interactive)
  (anything
   `((name . "cui-about.me/users")
     (init . cui-about:init-users)
     (candidates-in-buffer)
     (action . cui-about:show))))


(provide 'cui-about-me)
