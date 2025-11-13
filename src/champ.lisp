(in-package :champ)

;;; =============================================================================
;;; Hash map internals

(defconstant hash-bits-per-level
  ;; Drops to 4 on 32-bit implementations.
  (min 5 (- (integer-length (1+ (* 2 (integer-length most-positive-fixnum)))) 2)))
(defconstant node-radix (ash 1 hash-bits-per-level))
(defconstant hash-level-mask (1- node-radix))
(deftype bit-index () `(integer 0 ,(1- node-radix)))

(declaim (inline 1-bits-below))
(defun 1-bits-below (idx mask)
  "The number of 1 bits in `mask' at bit positions (strictly) less than `idx'."
  (declare (optimize (speed 3))
	   (type bit-index idx)
	   (fixnum mask))
  (logcount (logand (1- (the fixnum (ash 1 idx))) mask)))

;;; Layout of a map node:
;;;   Header (entry mask, subnode mask, size)
;;;   Key 0       \
;;;   Value 0     |
;;;   Key 1       |  Number of pairs is (logcount entry-mask)
;;;   Value 1     |
;;;   ...         /
;;;   ...         \
;;;   Subnode 1   |  Number of subnodes is (logcount subnode-mask);  note reversed order
;;;   Subnode 0   /
(defstruct (map-node
	     (:type vector))
  (entry-mask 0 :type fixnum)
  (subnode-mask 0 :type fixnum)
  (size 0 :type fixnum)) ; total number of pairs at or below this node
(defconstant map-node-header-size 3)

(declaim (inline map-node-entry-key map-node-entry-value))
(defun map-node-entry-key (node ientry)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector node)
	   (type bit-index ientry))
  (svref node (+ map-node-header-size (* 2 ientry))))
(defun map-node-entry-value (node ientry)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector node)
	   (type bit-index ientry))
  (svref node (1+ (+ map-node-header-size (* 2 ientry)))))

(declaim (inline map-node-subnode))
(defun map-node-subnode (node isubnode)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector node)
	   (type bit-index isubnode))
  (svref node (- (length node) isubnode 1)))

(deftype map-tree ()
  '(or null cons simple-vector))

(declaim (ftype (function (map-tree) fixnum) map-tree-size))
(declaim (inline map-tree-size))
(defun map-tree-size (tree)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((null tree) 0)
	((consp tree) (cadr tree))
	(t (map-node-size tree))))

;;; Assumptions:
;;; `hash-fn' returns fixnums (negative is OK)
(defun map-tree-with (tree key value hash-fn test)
  (declare (optimize (speed 3) (safety 1))
	   (type function hash-fn test))
  (let ((key-hash (funcall hash-fn key)))
    (declare (fixnum key-hash))
    (labels ((rec (node hash-shifted depth)
	       (declare (fixnum hash-shifted)
			(type (integer 0 64) depth))
	       (let ((hash-bits (logand hash-shifted hash-level-mask)))
		 (if (null node)
		     (vector (ash 1 hash-bits) 0 1 key value)
		   (if (consp node)
		       ;; A collision node is a list (hash size . alist).
		       (let ((coll-hash-shifted (ash (the fixnum (car node)) (* (- hash-bits-per-level) depth)))
			     (size (1+ (the fixnum (cadr node)))))
			 (declare (fixnum coll-hash-shifted size))
			 (if (= hash-shifted coll-hash-shifted)
			     ;; Update the collision node.
			     (let ((pair (assoc key (cddr node) :test test)))
			       (if pair
				   (if (hush (eql (cdr pair) value))
				       node ; no update needed
				     (list* (car node) (cadr node) ; update value; hash and size are unchcanged
					    (cons key value) (remove pair (cddr node) :test #'eq)))
				 ;; New entry in collision node.
				 (list* (car node) (1+ (the fixnum (cadr node)))
					(cons key value) (cddr node))))
			   (let ((hash-bits (logand hash-shifted hash-level-mask))
				 (coll-hash-bits (logand coll-hash-shifted hash-level-mask)))
			     (if (= hash-bits coll-hash-bits)
				 ;; Same index at this node; make a subnode.
				 (vector 0 (ash 1 hash-bits) size
					 (rec node (ash hash-shifted (- hash-bits-per-level)) (1+ depth)))
			       ;; Different indices at this node.
			       (vector (ash 1 hash-bits) (ash 1 coll-hash-bits) size key value node)))))
		     ;; Normal case.
		     (let* ((entry-mask (map-node-entry-mask node))
			    (subnode-mask (map-node-subnode-mask node))
			    (entry-idx (+ map-node-header-size (* 2 (1-bits-below hash-bits entry-mask))))
			    (subnode-idx (- (length node) 1 (1-bits-below hash-bits subnode-mask))))
		       (if (logbitp hash-bits entry-mask)
			   ;; Entry found
			   (let ((ex-key (svref node entry-idx))
				 (ex-val (svref node (1+ entry-idx))))
			     (if (funcall test key ex-key)
				 (if (hush (eql value ex-val))
				     ;; Key found, value equal: nothing to do
				     node
				   ;; Key found, value differs: just update value
				   (vector-update node (1+ entry-idx) value))
			       ;; Entry with different key found: make a subnode
			       (let* ((hash-shifted (ash hash-shifted (- hash-bits-per-level)))
				      (ex-key-hash (the fixnum (funcall hash-fn ex-key)))
				      (ex-key-hash-shifted (ash ex-key-hash (* (- hash-bits-per-level) (1+ depth))))
				      (n2 (if (= hash-shifted ex-key-hash-shifted)
					      ;; Collision!
					      (list key-hash 2 (cons key value) (cons ex-key ex-val))
					    ;; Turn the existing entry into a subnode and recur.
					    (rec (vector (ash 1 (logand ex-key-hash-shifted hash-level-mask))
							 0 1 ex-key ex-val)
						 hash-shifted (1+ depth))))
				      ;; The `1+' is because we're inserting _after_ `subnode-idx', because the subnodes
				      ;; are in reverse order.
				      (n (vector-rem-2-ins-1 node entry-idx (1+ subnode-idx) n2)))
				 (logandc2f (map-node-entry-mask n) (ash 1 hash-bits))
				 (logiorf (map-node-subnode-mask n) (ash 1 hash-bits))
				 (incf (map-node-size n))
				 n)))
			 ;; No entry found: check for subnode
			 (if (logbitp hash-bits subnode-mask)
			     ;; Subnode found
			     (let* ((subnode (svref node subnode-idx))
				    (new-subnode (rec subnode (ash hash-shifted (- hash-bits-per-level))
						      (1+ depth))))
			       (if (eq new-subnode subnode)
				   node ; no change
				 ;; New subnode
				 (let ((n (vector-update node subnode-idx new-subnode)))
				   ;; If only a value was updated, the subnode won't have changed size.
				   (setf (map-node-size n)
					 (the fixnum (+ (map-node-size n) (- (map-tree-size new-subnode)
									     (map-tree-size subnode)))))
				   n)))
			   ;; Neither entry nor subnode found: make new entry
			   (let ((n (vector-insert-2 node entry-idx key value)))
			     (logiorf (map-node-entry-mask n) (ash 1 hash-bits))
			     (incf (map-node-size n))
			     n)))))))))
      (rec tree key-hash 0))))

(defun map-tree-less (tree key hash-fn test)
  (declare (optimize (speed 3) (safety 1))
	   (type function hash-fn test))
  (let ((key-hash (funcall hash-fn key)))
    (declare (fixnum key-hash))
    (labels ((rec (node hash-shifted depth)
	       (declare (fixnum hash-shifted)
			(type (integer 0 64) depth))
	       (and node
		    (if (consp node)
			(if (/= (the fixnum (car node)) key-hash)
			    (values node nil)
			  (let ((pair (assoc key (cddr node) :test test)))
			    (if pair
				(let ((new-alist (remove pair (cddr node) :test #'eq)))
				  (values (if (null (cdr new-alist))
					      ;; Only one pair left; convert back to regular node
					      (vector (ash 1 (logcount (logand hash-shifted hash-level-mask)))
						      0 1 (caar new-alist) (cdar new-alist))
					    (list* (car node) (1- (the fixnum (cadr node))) new-alist))
					  (cdr pair)))
			      (values node nil))))
		      (let* ((hash-bits (logand hash-shifted hash-level-mask))
			     (entry-mask (map-node-entry-mask node))
			     (subnode-mask (map-node-subnode-mask node))
			     (entry-idx (+ map-node-header-size (* 2 (1-bits-below hash-bits entry-mask)))))
			(if (logbitp hash-bits entry-mask)
			    (let ((ex-key (svref node entry-idx)))
			      (cond ((not (funcall test ex-key key))
				     (values node nil))
				    ((and (= (logcount entry-mask) 1) (= 0 subnode-mask))
				     ;; Only the root node is allowed to contain one entry and zero subnodes.
				     (assert (= depth 0))
				     (values nil (svref node (1+ entry-idx))))
				    (t
				     (let ((n (vector-remove-2 node entry-idx)))
				       (logandc2f (map-node-entry-mask n) (ash 1 hash-bits))
				       (decf (map-node-size n))
				       (values n (svref node (1+ entry-idx)))))))
			  (let* ((subnode-raw-idx (1-bits-below hash-bits subnode-mask))
				 (subnode-idx (- (the fixnum (length node)) 1 subnode-raw-idx)))
			    (declare (fixnum subnode-raw-idx subnode-idx))
			    (if (not (logbitp hash-bits subnode-mask))
				(values node nil) ; there's no subnode containing `key'
			      (let ((subnode (svref node subnode-idx)))
				(multiple-value-bind (new-subnode lookup-val)
				    (rec subnode (ash hash-shifted (- hash-bits-per-level)) (1+ depth))
				  (if (eq new-subnode subnode)
				      (values node nil)
				    (let ((n (cond ((consp new-subnode)
						    (vector-update node subnode-idx new-subnode))
						   ((and (= 1 (logcount (map-node-entry-mask new-subnode)))
							 (= 0 (map-node-subnode-mask new-subnode)))
						    ;; New subnode contains only an entry: pull it up into this node
						    (let* ((new-entry-idx
							     (+ map-node-header-size
								(* 2 (1-bits-below hash-bits entry-mask))))
							   (k (map-node-entry-key new-subnode 0))
							   (v (map-node-entry-value new-subnode 0))
							   (n (vector-ins-2-rem-1 node new-entry-idx k v subnode-idx)))
						      (logiorf (map-node-entry-mask n) (ash 1 hash-bits))
						      (logandc2f (map-node-subnode-mask n) (ash 1 hash-bits))
						      n))
						   ((and (= 0 (map-node-entry-mask new-subnode))
							 (= 1 (logcount (map-node-subnode-mask new-subnode)))
							 (consp (svref new-subnode map-node-header-size)))
						    ;; New subnode contains only a collision node: pull it up
						    (vector-update node subnode-idx
								   (svref new-subnode map-node-header-size)))
						   (t (vector-update node subnode-idx new-subnode)))))
				      (decf (map-node-size n))
				      (values n lookup-val)))))))))))))
      (rec tree key-hash 0))))

(defun map-tree-lookup (tree key hash-fn test)
  (declare (optimize (speed 3) (safety 0))
	   (type function hash-fn test))
  (let ((key-hash (funcall hash-fn key)))
    (declare (fixnum key-hash))
    (labels ((rec (node key hash-shifted)
	       (declare (fixnum hash-shifted))
	       (and node
		    (if (consp node)
			(let ((pair (assoc key (cddr node) :test #'eq)))
			  (and pair (values (cdr pair) t)))
		      (let ((hash-bits (logand hash-shifted hash-level-mask))
			    (entry-mask (map-node-entry-mask node)))
			(if (logbitp hash-bits entry-mask)
			    (let* ((entry-idx (+ map-node-header-size
						(* 2 (1-bits-below hash-bits entry-mask))))
				   (ex-key (svref node entry-idx)))
			      (and (funcall test ex-key key)
				   (values (svref node (1+ entry-idx)) t)))
			  (let ((subnode-mask (map-node-subnode-mask node)))
			    (and (logbitp hash-bits subnode-mask)
				 (let* ((subnode-idx (- (the fixnum (length node))
							1 (1-bits-below hash-bits subnode-mask)))
					(subnode (svref node subnode-idx)))
				   (rec subnode key (ash hash-shifted (- hash-bits-per-level))))))))))))
      (rec tree key key-hash))))

(defun vector-update (vec idx val)
  "Returns a new vector like `vec' but with `val' at `idx'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum idx))
  (let* ((len (length vec))
	 (new-vec (make-array len)))
    (declare (fixnum len))
    (dotimes (i len)
      (setf (svref new-vec i) (svref vec i)))
    (setf (svref new-vec idx) val)
    new-vec))

(defun vector-insert-2 (vec idx ins-0 ins-1)
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum idx))
  (let* ((len (length vec))
	 (v (make-array (+ 2 len))))
    (declare (fixnum len))
    (dotimes (i idx)
      (setf (svref v i) (svref vec i)))
    (setf (svref v idx) ins-0)
    (setf (svref v (1+ idx)) ins-1)
    (dotimes (i (- len idx))
      (setf (svref v (+ idx i 2)) (svref vec (+ idx i))))
    v))

(defun vector-rem-2-ins-1 (vec rem-idx ins-idx ins-val)
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum rem-idx ins-idx))
  (assert (<= rem-idx (the fixnum (- ins-idx 2))))
  (let* ((len (length vec))
	 (v (make-array (1- len))))
    (declare (fixnum len))
    (dotimes (i rem-idx)
      (setf (svref v i) (svref vec i)))
    (dotimes (i (- ins-idx rem-idx 2))
      (declare (fixnum i))
      (setf (svref v (+ rem-idx i)) (svref vec (+ rem-idx i 2))))
    (setf (svref v (- ins-idx 2)) ins-val)
    (dotimes (i (- len ins-idx))
      (declare (fixnum i))
      (setf (svref v (+ ins-idx i -1)) (svref vec (+ ins-idx i))))
    v))

(defun vector-remove-2 (vec idx)
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum idx))
  (let* ((len (length vec))
	 (v (make-array (- len 2))))
    (declare (fixnum len))
    (dotimes (i idx)
      (setf (svref v i) (svref vec i)))
    (dotimes (i (- len idx 2))
      (declare (fixnum i))
      (setf (svref v (+ idx i)) (svref vec (+ idx i 2))))
    v))

(defun vector-ins-2-rem-1 (vec ins-idx ins-0 ins-1 rem-idx)
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum ins-idx rem-idx))
  (assert (<= ins-idx rem-idx))
  (let* ((len (length vec))
	 (v (make-array (1+ (length vec)))))
    (declare (fixnum len))
    (dotimes (i ins-idx)
      (setf (svref v i) (svref vec i)))
    (setf (svref v ins-idx) ins-0)
    (setf (svref v (1+ ins-idx)) ins-1)
    (dotimes (i (- rem-idx ins-idx))
      (setf (svref v (+ ins-idx i 2)) (svref vec (+ ins-idx i))))
    (dotimes (i (- len rem-idx 1))
      (declare (fixnum i))
      (setf (svref v (the fixnum (+ rem-idx i 2))) (svref vec (+ rem-idx i 1))))
    v))


;;; ----------------
;;; Iterator

(defun make-map-tree-iterator-internal (tree)
  (declare (optimize (speed 3) (safety 0)))
  (let ((iter (make-array (+ 1 (* 2 (ceiling (integer-length most-positive-fixnum) hash-bits-per-level))))))
    (if (null tree)
	(setf (svref iter 0) -1)
      (progn
	(setf (svref iter 0) 1) ; the stack pointer
	(setf (svref iter 1) tree)
	(setf (svref iter 2) 0)
	(map-tree-iterator-canonicalize iter)))
    iter))

(defun map-tree-iterator-canonicalize (iter)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((sp (svref iter 0))
	 (node (svref iter sp))
	 (idx (svref iter (1+ sp))))
    (declare (type (or null fixnum) idx))
    (when (if (null idx)
	      (null (svref iter sp))
	    (= idx (logcount (logior (map-node-entry-mask node) (map-node-subnode-mask node)))))
      (decf sp 2)
      (unless (< sp 0)
	(setq node (svref iter sp))
	(setq idx (svref iter (1+ sp)))))
    (unless (< sp 0)
      (loop
	(when (null idx)
	  (return))
	(let ((n-entries (logcount (map-node-entry-mask node)))
	      (n-subnodes (logcount (map-node-subnode-mask node))))
	  (when (< idx n-entries)
	    (return))
	  (let ((len (length (the simple-vector node))))
	    (setq node (svref node (- len (- idx n-entries) 1)))
	    (incf idx)
	    (unless (= idx (+ n-entries n-subnodes)) ; TCO
	      (setf (svref iter (1+ sp)) idx)
	      (incf sp 2))
	    (if (consp node)
		(setq node (cddr node)
		      idx nil)
	      (setq idx 0))
	    (setf (svref iter sp) node)
	    (setf (svref iter (1+ sp)) idx)))))
    (setf (svref iter 0) sp)))

(defun map-tree-iterator-done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (< (the fixnum (svref iter 0)) 0))

(defun map-tree-iterator-get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (let ((sp (svref iter 0)))
    (declare (fixnum sp))
    (and (> sp 0)
	 (let ((node (svref iter sp))
	       (idx (svref iter (1+ sp))))
	   (declare (type (or null fixnum) idx))
	   (if (null idx)
	       (multiple-value-bind (key val)
		   (let ((pair (pop (svref iter sp))))
		     (values (car pair) (cdr pair) t))
		 (map-tree-iterator-canonicalize iter)
		 (values key val t))
	     (let ((entry-idx (+ map-node-header-size (the fixnum (* 2 idx)))))
	       (setf (svref iter (1+ sp)) (1+ idx))
	       (map-tree-iterator-canonicalize iter)
	       (values (svref node entry-idx) (svref node (1+ entry-idx))
		       t)))))))

(defun map-tree-verify (tree key-hash-fn)
  (declare (optimize (debug 3)))
  (macrolet ((test (form)
	       `(or ,form
		    (progn
		      (cerror "Ignore and proceed."
			      "Verification check failed at ~:A: ~S" (path depth partial-hash) ',form)
		      t))))
    (or (null tree)
	(labels ((rec (node depth partial-hash)
		   (let* ((entry-mask (map-node-entry-mask node))
			  (subnode-mask (map-node-subnode-mask node))
			  (size (logcount entry-mask)))
		     (and (test (= 0 (logand entry-mask subnode-mask)))
			  (test (= 0 (ash entry-mask (- node-radix))))
			  (test (= 0 (ash subnode-mask (- node-radix))))
			  (test (= (length node)
				   (+ map-node-header-size (* 2 (logcount entry-mask)) (logcount subnode-mask))))
			  ;; Check that unless root, this node does not contain only an entry ...
			  (test (not (and (> depth 0) (= 1 (logcount entry-mask)) (= 0 subnode-mask))))
			  ;; ... or only a collision subnode
			  (test (not (and (> depth 0) (= 0 entry-mask) (= 1 (logcount subnode-mask))
					  (consp (svref node (1- (length node)))))))
			  ;; Check entry key hashes
			  (let ((entry-idx 0))
			    (every (lambda (hash-bits)
				     (let ((key (svref node (+ map-node-header-size (* 2 entry-idx)))))
				       (incf entry-idx)
				       (test (= (ldb (byte (* hash-bits-per-level (1+ depth)) 0)
						     (funcall key-hash-fn key))
						(new-partial-hash hash-bits depth partial-hash)))))
				   (bit-indices entry-mask)))
			  ;; Verify subnodes
			  (let ((subnode-idx 0))
			    (every (lambda (hash-bits)
				     (let ((subnode (svref node (- (length node) subnode-idx 1))))
				       (incf subnode-idx)
				       (if (consp subnode)
					   (let ((key-hash (funcall key-hash-fn (caaddr subnode))))
					     (and (test (= (car subnode) key-hash))
						  (test (= (cadr subnode) (length (cddr subnode))))
						  (incf size (cadr subnode))
						  (every (lambda (pair)
							   (test (= (funcall key-hash-fn (car pair)) key-hash)))
							 (cddr subnode))))
					 (and (rec subnode (1+ depth) (new-partial-hash hash-bits depth partial-hash))
					      (progn
						(incf size (map-node-size subnode))
						t)))))
				   (bit-indices subnode-mask)))
			  ;; Finally, check size
			  (test (= (map-node-size node) size)))))
		 (new-partial-hash (hash-bits depth partial-hash)
		   (dpb hash-bits (byte hash-bits-per-level (* hash-bits-per-level depth))
			partial-hash))
		 (path (depth partial-hash)
		   (let ((path nil))
		     (dotimes (i depth)
		       (push (ldb (byte hash-bits-per-level (* i hash-bits-per-level))
				  partial-hash)
			     path))
		     (nreverse path))))
	  (rec tree 0 0)))))

;;; Verification utility, used to recover hash bits from masks.
(defun bit-indices (mask)
  "A list of the indices of the 1 bits of `mask', in ascending order."
  (let ((result nil))
    (do-bit-indices (i mask (nreverse result))
      (push i result))))


;;; ================================================================
;;; API

;;; A persistent functional map.
(defstruct (champ-map
	     (:constructor raw-make-champ-map (contents hash-fn test))
	     (:print-function print-map))
  (contents nil :type map-tree :read-only t)
  (hash-fn nil :type function :read-only t)
  (test nil :type function :read-only t))

(defun make-map (hash-fn test)
  "Creates a persistent functional map with the given `hash-fn' and `test'."
  (raw-make-champ-map nil (coerce hash-fn 'function) (coerce test 'function)))

(defun map-size (map)
  "Returns the number of entries in `map'."
  (declare (type champ-map map))
  (map-tree-size (champ-map-contents map)))

(defun map-lookup (map key)
  "If `map' has an entry for `key', returns the associated value and a true
second value; otherwise returns `nil'."
  (declare (type champ-map map))
  (map-tree-lookup (champ-map-contents map) key (champ-map-hash-fn map) (champ-map-test map)))

(defun map-with (map key value)
  "Returns a new map which has all the entries of `map', except that an entry
has been added or updated associating `key' with `value'."
  (declare (type champ-map map))
  (let* ((contents (champ-map-contents map))
	 (hash-fn (champ-map-hash-fn map))
	 (test (champ-map-test map))
	 (new-contents (map-tree-with contents key value hash-fn test)))
    (if (eq new-contents contents)
	map
      (raw-make-champ-map new-contents hash-fn test))))

(defun map-less (map key)
  "Returns a new map which has all the entries of `map' except the one for
`key', if any."
  (declare (type champ-map map))
  (let* ((contents (champ-map-contents map))
	 (hash-fn (champ-map-hash-fn map))
	 (test (champ-map-test map))
	 (new-contents (map-tree-less contents key hash-fn test)))
    (if (eq new-contents contents)
	map
      (raw-make-champ-map new-contents hash-fn test))))

;;; This is just one way to wrap the internal iterator.  Feel free to do it differently.
(defun make-map-iterator (map)
  "Returns an iterator for `map' implemented as a closure.  Invoking it on
`:get', if the iterator has pairs left, returns the next pair as two values,
key and value.  Invoking it on `:done?' will return true iff the iterator is
exhausted; on `:more?', true iff the iterator has pairs left."
  (declare (type champ-map map))
  (let ((iter (make-map-tree-iterator-internal (champ-map-contents map))))
    (lambda (op)
      (ecase op
	(:get (map-tree-iterator-get iter))
	(:done? (map-tree-iterator-done? iter))
	(:more? (not (map-tree-iterator-done? iter)))))))

(defun print-map (map stream level)
  (declare (ignore level))
  (format stream "#<~S, ~D entries>" 'champ-map (map-size map)))


;;; ----------------
;;; Mutable hash table

(defstruct (champ-table
	     (:constructor raw-make-champ-table (contents hash-fn test lock))
	     (:print-function print-table))
  (contents nil :type map-tree)
  (hash-fn nil :type function :read-only t)
  (test nil :type function :read-only t)
  (lock nil :read-only t))

(defun make-table (hash-fn test &key synchronized?)
  (raw-make-champ-table nil (coerce hash-fn 'function) (coerce test 'function)
			(and synchronized? (make-lock))))

(defun table-empty? (table)
  (declare (type champ-table table))
  (zerop (map-tree-size (champ-table-contents table))))

(defun table-size (table)
  "The number of entries in `table'."
  (declare (type champ-table table))
  (map-tree-size (champ-table-contents table)))

(defun table-get (table key)
  (declare (type champ-table table))
  (map-tree-lookup (champ-table-contents table) key (champ-table-hash-fn table) (champ-table-test table)))

(defun table-put (table key value)
  (declare (type champ-table table))
  (let ((contents (champ-table-contents table))
	(hash-fn (champ-table-hash-fn table))
	(test (champ-table-test table))
	(lock (champ-table-lock table)))
    (if lock
	(with-lock (lock)
	  (setf (champ-table-contents table)
		(map-tree-with contents key value hash-fn test)))
      (setf (champ-table-contents table)
	    (map-tree-with contents key value hash-fn test)))))

(defun table-remove (table key)
  (declare (type champ-table table))
  (let ((contents (champ-table-contents table))
	(hash-fn (champ-table-hash-fn table))
	(test (champ-table-test table))
	(lock (champ-table-lock table)))
    (if lock
	(with-lock (lock)
	  (setf (champ-table-contents table)
		(map-tree-less contents key hash-fn test)))
      (setf (champ-table-contents table)
	    (map-tree-less contents key hash-fn test)))))

(defun make-table-iterator (table)
  (declare (type champ-table table))
  (let ((iter (make-map-tree-iterator-internal (champ-table-contents table))))
    (lambda (op)
      (ecase op
	(:get (map-tree-iterator-get iter))
	(:done? (map-tree-iterator-done? iter))
	(:more? (not (map-tree-iterator-done? iter)))))))

(defun print-table (table stream level)
  (declare (ignore level))
  (format stream "#<~S, ~D entries>" 'champ-table (table-size table)))

