(in-package :com.sympoiesis.champ)


;;; ----------------
;;; Internals

;;; These are macros because Allegro (still!) doesn't inline user functions.
(defmacro hash-mix (&rest args)
  "Returns the \"mix\" of the values, where \"mix\" is a commutative and
associative operation with an inverse.  All values MUST be fixnums; the
result is a fixnum."
  ;; On implementations where we can reliably get fixnum addition and subtraction without
  ;; overflow checks at speed-3 safety-0, using them will give us better distributional properties.
  ;; -- Oops, this doesn't quite fly on SBCL, not because it's wrong per se, but because of
  ;; `sb-ext:restrict-compiler-policy', which lets people override my declarations.  Ironic.
  ;; SBCL does provide another way to do it, which, however, discards the sign bit.  Oh well.
  #+sbcl
  `(locally (declare (optimize (speed 3) (safety 0)))  ; still desirable when possible
     (logand most-positive-fixnum (+ . ,(mapcar (lambda (x) `(the fixnum ,x)) args))))
  ;; On other implementations, we fall back to XOR.
  #+(or ccl allegro lispworks)
  ;; We have to build a binary tree with `the fixnum' at each level.
  (labels ((build (fn expr args)
	     (if (null args) expr
	       (build fn `(the fixnum (,fn ,expr (the fixnum ,(car args)))) (cdr args)))))
    `(locally (declare (optimize (speed 3) (safety 0)))
       ,(build '+ `(the fixnum ,(car args)) (cdr args))))
  ;; Oddly, addition doesn't seem to work on ABCL, even though it's using an `ladd' instruction; it
  ;; makes a bignum (given suitable operands) anyway.
  ;; CLASP also checks for overflow on addition.
  #-(or sbcl ccl allegro lispworks)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (the fixnum (logxor . ,(mapcar (lambda (x) `(the fixnum ,x)) args)))))

(defmacro hash-unmix (hash &rest to-unmix)
  "Returns the result of \"unmixing\" each of `to-unmix' from `hash'.  All
values MUST be fixnums; the result is a fixnum."
  ;; As above.
  #+sbcl
  `(locally (declare (optimize (speed 3) (safety 0)))  ; still desirable when possible
     (logand most-positive-fixnum (- (the fixnum ,hash) . ,(mapcar (lambda (x) `(the fixnum ,x)) to-unmix))))
  #+(or ccl allegro lispworks)
  (labels ((build (fn expr args)
	     (if (null args) expr
	       (build fn `(the fixnum (,fn ,expr (the fixnum ,(car args)))) (cdr args)))))
    `(locally (declare (optimize (speed 3) (safety 0)))
       ,(build '- `(the fixnum ,hash) to-unmix)))
  #-(or sbcl ccl allegro lispworks)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (the fixnum (logxor (the fixnum ,hash) . ,(mapcar (lambda (x) `(the fixnum ,x)) to-unmix)))))

(define-modify-macro hash-mixf (&rest args)
  hash-mix)
(define-modify-macro hash-unmixf (hash &rest to-unmix)
  hash-unmix)

(define-modify-macro logiorf (&rest args)
  logior)

(define-modify-macro logandc2f (arg-2)
  logandc2)

(defmacro hush (&body body)
  `(locally
       (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
     . ,body))

(defmacro do-bit-indices ((idx-var val &optional result) &body body)
  (let ((val-var (gensym "VAL-")))
    `(let ((,val-var ,val))
       (declare (fixnum ,val-var))
       (do ()
	   ((= 0 ,val-var) ,result)
	 (let ((,idx-var (1- (logcount (logxor ,val-var (1- ,val-var))))))
	   (logandc2f ,val-var (ash 1 ,idx-var))
	   . ,body)))))


;;; ----------------
;;; Portable locking

#+(and sbcl sb-thread)
(progn
  (defun make-lock (&optional name)
    (apply #'sb-thread:make-mutex (and name `(:name ,name))))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(sb-thread:with-mutex (,lock :wait-p ,wait?)
       . ,body)))

#+(and sbcl (not sb-thread))
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    nil)
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore lock wait?))
    `(progn
       . ,body)))


#+openmcl
(progn
  (defun make-lock (&optional name)
    (ccl:make-lock name))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (let ((lock-var (gensym "LOCK-"))
	  (wait?-var (gensym "WAIT?-"))
	  (try-succeeded?-var (gensym "TRY-SUCCEEDED?-")))
      `(let ((,lock-var ,lock)
	     . ,(and (not (eq wait? 't))
		     `((,wait?-var ,wait?)
		       (,try-succeeded?-var nil))))
	 ,(if (eq wait? 't)
	      `(ccl:with-lock-grabbed (,lock-var)
		. ,body)
	    `(unwind-protect
		 (and (or ,wait?-var (and (ccl:try-lock ,lock-var)
					  (setq ,try-succeeded?-var t)))
		      (ccl:with-lock-grabbed (,lock-var)
			. ,body))
	       (when ,try-succeeded?-var
		 (ccl:release-lock ,lock-var))))))))


#+(and clasp threads)
(progn
  (defun make-lock (&optional name)
    (mp:make-lock :name (or name :anonymous)))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore wait?))
    `(mp:with-lock (,lock) ,@body)))


#+(and ecl (not threads))
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    nil)
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore lock wait?))
    `(progn . ,body)))

#+(and ecl threads)
(progn
  (defun make-lock (&optional name)
    (apply #'mp:make-lock :recursive t (and name `(:name ,name))))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (let ((lock-var (gensym "LOCK-"))
	  (wait?-var (gensym "WAIT?-"))
	  (try-succeeded?-var (gensym "TRY-SUCCEEDED?-")))
      `(let ((,lock-var ,lock)
	     . ,(and (not (eq wait? 't))
		     `((,wait?-var ,wait?)
		       (,try-succeeded?-var nil))))
	 ,(if (eq wait? 't)
	      `(mp:with-lock (,lock-var)
		. ,body)
	    `(unwind-protect
		 (and (or ,wait?-var (and (mp:get-lock ,lock-var nil)
					  (setq ,try-succeeded?-var t)))
		      (mp:with-lock (,lock-var)
			. ,body))
  	       (when ,try-succeeded?-var
		 (mp:giveup-lock ,lock-var))))))))


#+abcl
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    (threads:make-mutex))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore wait?))
    `(threads:with-mutex (,lock)
       . ,body)))


#+cmu
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    nil)
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore lock wait?))
    `(sys:without-interrupts . ,body)))


#+allegro
(progn
  (defun make-lock (&optional name)
    (apply #'mp:make-process-lock (and name `(:name ,name))))
  ;; If `wait?' is false, and the lock is not available, returns without executing the body.
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(mp:with-process-lock (,lock :timeout ,(cond ((eq wait? 't) nil)  ; hush, Allegro
						  ((eq wait? 'nil) 0)
						  (t `(if ,wait? nil 0))))
       . ,body)))


#+lispworks
(progn
  (defun make-lock (&optional name)
    (apply #'mp:make-lock (and name `(:name ,name))))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(mp:with-lock (,lock :timeout (if ,wait? nil 0))
       . ,body)))


#+scl
(progn
  (defun make-lock (&optional name)
    (thread:make-lock name :type ':recursive :auto-free t))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(thread:with-lock-held (,lock "Lock Wait" :wait ,wait?)
       . ,body)))


#+(and genera new-scheduler)
(progn
  (defun make-lock (&optional name)
    (process:make-lock name))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore wait?))
    `(process:with-lock (,lock)
       . ,body)))

(defmacro with-lock-maybe ((lock &key (wait? t)) &body body)
  "If `lock' is nonnull, locks it around `body'; otherwise just executes `body'."
  (let ((lock-var (gensym "LOCK-"))
	(body-fn (gensym "BODY-")))
    `(let ((,lock-var ,lock))
       (flet ((,body-fn ()
		. ,body))
	 (if ,lock-var
	     (with-lock (,lock-var :wait? ,wait?)
	       (,body-fn))
	   (,body-fn))))))


;;; ----------------
;;; Iteration macros

(defmacro do-map ((key-var value-var map &optional value)
		  &body body)
  (let ((map-var (gensym "MAP-"))
	(body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-")))
    `(let ((,map-var ,map))
       (declare (type champ-map ,map-var))
       (labels ((,body-fn (,key-var ,value-var)
		  . ,body)
		(,recur-fn (tree)
		  (when tree
		    (if (consp tree)
			(dolist (pair (cddr tree))
			  (,body-fn (car pair) (cdr pair)))
		      (let ((len (the fixnum (length (the simple-vector tree)))))
			(dotimes (i (logcount (map-node-entry-mask tree)))
			  (let ((idx (+ (* 2 i) map-node-header-size)))
			    (,body-fn (svref tree idx) (svref tree (1+ idx)))))
			(dotimes (i (logcount (map-node-subnode-mask tree)))
			  (,recur-fn (svref tree (- len 1 i)))))))))
	 (declare (inline ,body-fn))
	 (,recur-fn (champ-map-contents ,map-var)))
       ,value)))

(defmacro do-table ((key-var value-var table &optional value)
		    &body body)
  (let ((table-var (gensym "TABLE-"))
	(body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-")))
    `(let ((,table-var ,table))
       (declare (type champ-table ,table-var))
       (labels ((,body-fn (,key-var ,value-var)
		  . ,body)
		(,recur-fn (tree)
		  (when tree
		    (if (consp tree)
			(dolist (pair (cddr tree))
			  (,body-fn (car pair) (cdr pair)))
		      (let ((len (the fixnum (length (the simple-vector tree)))))
			(dotimes (i (logcount (map-node-entry-mask tree)))
			  (let ((idx (+ (* 2 i) map-node-header-size)))
			    (,body-fn (svref tree idx) (svref tree (1+ idx)))))
			(dotimes (i (logcount (map-node-subnode-mask tree)))
			  (,recur-fn (svref tree (- len 1 i)))))))))
	 (declare (inline ,body-fn))
	 (,recur-fn (champ-table-contents ,table-var)))
       ,value)))
