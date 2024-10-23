(provide 'tel)

;; tel-make-shortcut: define a function tel-shortcut-<name> that calls
;; (action) if (predicate) is *false*. Otherwise, it switches to the
;; buffer which was previously used.
(defmacro tel-make-shortcut (name action predicate)
  (let* ((buf-store-name
	   (concat "tel-shortcut--" name "-buffer"))
	  (invoke-name-str (concat "tel-shortcut-" name))

	  (buf-store (intern buf-store-name)))

    `(progn
       (setq ,buf-store nil)
       (defalias (intern ,invoke-name-str)
	 (lambda ()
	   (interactive)
	   (if (,predicate)
	       (progn
		 (switch-to-buffer ,buf-store)
		 (setq ,buf-store nil))
	     (progn
	       (setq ,buf-store (current-buffer))
	       (,action))))))))

;; Tag a given string with the current date.
(defun tel--date-fname (name)
  (let ((suffix (format-time-string "%m-%d")))
    (concat name "-" suffix)))

;; Given a list of file names, prompt the user to select one and
;; create or open a file with that name and the current date. Open it
;; for editing.
(defun tel-open-dated-file (prompt names path-prefix path-suffix)
  (let ((name (completing-read prompt names)))
    (find-file
     (concat path-prefix "/" (tel--date-fname name) path-suffix))))


(defun tel-no-del--should-del-from ()
  (let ((text (thing-at-point 'line t)))
    (cond ((string= text "# del on\n")  t)

	  ((string= text "# del off\n") nil)
	  ((= (line-number-at-pos) 1) t)
	  (t (progn
	       (previous-line)
	       (tel-no-del--should-del-from))))))

;; TODO: search upwards for # del off, # del on. if
;; nothing or del on, return t.
(defun tel-no-del--should-del ()
  (save-excursion
    (progn
      (previous-line)
      (tel-no-del--should-del-from))))

(defun tel-no-del--bs ()
  (interactive)
  (when (tel-no-del--should-del)
      (delete-backward-char 1)))

;; tel-no-del-mode: Deleting text with backspace and C-backspace
;; is disabled between "# del off" and "# del on".
(define-minor-mode tel-no-del-mode
  "tel-no-del-mode selectively disables text deletion."
  nil " Tel/Del"
  '(([backspace] . tel-no-del--bs)))
