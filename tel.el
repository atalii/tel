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
