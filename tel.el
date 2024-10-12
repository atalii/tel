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
