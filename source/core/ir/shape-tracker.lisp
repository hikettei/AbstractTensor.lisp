
(in-package :AbstractTensor/ir)

(defstruct (AbstractTensor
	    (:conc-name aten-)
	    (:constructor
		make-aten
		(id type-class shape order where)))
  "
A{T}[M, ~, N]<1 ~ 0>(where M = 1)
^ ^  ^          ^      ^- where syntax
| |  |          |---- memory order
| |  |- Shape, ~ means broadcast. [] to express scalar value.
| |- One of: T, Dense, Sparse, Complex, and (U)Int4~64, FP16, FP32, FP64. or arbitary symbol. (AutoInfer)
|--- The name of tensor.
"
  (id (or id (gensym "TID")) :type symbol)
  (type-class type-class)
  (shape shape :type list)
  (order order :type list)
  (where where))

(define-condition aten-parse-error ()
  ((expression :type string)
   (at :type keyword))

  )

;; TODO: Improve the quality of error message.
(declaim (ftype (function (string) AbstractTensor) parse-aten %parse-aten))
(defun %parse-aten (expression
		    &aux
		      (brackets-start '(#\{ #\[ #\< #\())
		      (brackets-end   '(#\} #\] #\> #\))))
  "
A simple parser which intercepts AbstractTensor notation.

Example:
    ```
    (parse-aten \"A{T}[M ~ N]<1 ~ 0>(where M = 1)\")
    -> <Unsolved AbstractTensor>
    ```
"
  (declare (type simple-string expression)
	   (optimize (speed 3)))

  ;; Note that this function must be
  ;; heavily optimized because used anywhere
  
  ;; bracket1 = {
  ;; bracket2 = [
  ;; bracket3 = <
  ;; bracket4 = (

  ;; Assertions
  (multiple-value-bind
	(bracket-starts bracket-ends)
      (flet ((finder (x)
	       (declare (type standard-char x))
	       (position x expression)))
	(values
	 (map 'list #'finder brackets-start)
	 (map 'list #'finder brackets-end)))

    (macrolet ((? (conds comment)
		 `(when ,conds
		    (error "[~a] Aten parse error: ~a" ,comment expression))))
      
      (? (some #'null bracket-starts)
	 "missing bracket")
      (? (some #'null bracket-ends)
	 "missing bracket")
      (? (apply #'> (loop for s in bracket-starts
			  for e in bracket-ends
			  append (list s e)))
	 "mismatch pair")
      (? (not (equal (aref expression (1- (length expression))) (nth 3 brackets-end)))
	 "syntax error"))

    (multiple-value-bind
	  (name type shape stride where)
	(labels ((take (n)
		   (let ((s (or (and (= n 0) 0) (nth (1- n) bracket-starts)))
			 (e (or (nth n bracket-starts) (length expression))))
		     ;; the type of expression cannot be inferred
		     (xsubseq:xsubseq (the string expression) s e)))
		 (str (subseq)
		   (declare (type xsubseq:xsubseq subseq))
		   (xsubseq:coerce-to-string (xsubseq:xnconc subseq)))
		 
		 (parse (obj f) (declare (type function f)) (funcall f obj))
		 (name (str)
		   (declare (type string str))
		   ;; name = 0~9, a-z, A-Z, - or _
		   (if (cl-ppcre:all-matches "[0-9|a-z|A-Z|_|-]" str)
		       (the keyword (intern str "KEYWORD"))
		       nil))
		 (broadcast (str)
		   (declare (type (simple-array character (*)) str))
		   (if (string= str "~")
		       :~
		       nil))
		 (number (str)
		   (declare (type string str))
		   (if (cl-ppcre:all-matches "[0-9]" str)
		       (parse-integer str)
		       nil))
		 (subscript (index checker)
		   (declare (type function checker))
		   #'(lambda (str)
		       (declare (type xsubseq:xsubseq str))
		       (if (=
			    -2
			    (- (the fixnum (xsubseq::string-xsubseq-start str))
			       (the fixnum (xsubseq::string-xsubseq-end str))))
			   nil ;; Scalar
			   (loop with i fixnum = (1+ (the fixnum (xsubseq::string-xsubseq-start str)))
				 with str1 string = (str str)
				 with end fixnum = (length expression)
				 for pos = (min
					    (or (position (nth index brackets-end) expression :start i) end)
					    (or (position #\space expression :start i) end))
				 while (< pos (the fixnum (xsubseq::string-xsubseq-end str)))
				 collect
				 (prog1
				     (funcall checker (xsubseq:xsubseq (the string expression) i pos))
				   (setf i (1+ pos))))))))
	  (values
	   (parse
	    (take 0)
	    #'(lambda (x)
		(or
		 (name (str x))
		 (error "invaild name"))))
	   (parse
	    (take 1)
	    (subscript
	     0
	     #'(lambda (x &aux (x1 (str x)))
		 (or
		  (name x1)
		  (error "invaild type")))))
	   (parse
	    (take 2)
	    (subscript
	     1
	     #'(lambda (x &aux (x1 (str x)))
		 (or
		  (name x1)
		  (broadcast x1)
		  (error "cannot parse1 ~a" x1)))))
	   (parse
	    (take 3)
	    (subscript
	     2
	     #'(lambda (x &aux (x1 (str x)))
		 (or
		  (number x1)
		  (broadcast x1)
		  (error "cannot parse2 ~a" x1)))))
	   (let ((str (the (simple-array character (*)) (str (take 4)))))
	     (if (string= str "()")
		 nil
		 (read-from-string (cl-ppcre:regex-replace-all "=" str " = "))))))
      (declare (type list shape stride type where))

      (when (not (= (length shape) (length stride)))
	(error "Inconsistent dims"))

      (when (not (= (length type) 1))
	(error "Type must not include a whitespace"))

      ;; ===============================================================
      ;; TODO: BroadcastのPosition, Memory-Orderの確認もする
      ;; <Row> <Column>の指定で，自動でStride作成できるようにする。
      ;; ===============================================================

      (when (not (= 0 (mod (length where) 3)))
	(error "where parse error"))

      ;; TODO 
      (let ((where-pretty
	      (loop for i upfrom 0 below (length where) by 3
		    for name = (nth (+ 0 i) where)
		    for eq   = (nth (+ 1 i) where)
		    for exp  = (nth (+ 2 i) where)
		    if (eq '= eq)
		      collect (cons name exp)
		    else
		      do (error "="))))
	(make-aten name (car type) shape stride where-pretty)))))

(defun parse-aten (expression)
  (declare (inline %parse-aten))
  (%parse-aten expression))

(define-compiler-macro parse-aten (expression)
  (with-slots ((id id) (type-class type-class) (shape shape) (order order) (where where)) (%parse-aten expression)
    `(make-aten ',id ',type-class ',shape ',order ',where)))


