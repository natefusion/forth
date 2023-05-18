(defpackage forth (:use :cl))
(in-package :forth)

(defun read-code (&optional (file "test.fs"))
  (uiop:read-file-string file))

(defstruct scanner
  (source "" :type string)
  (start 0 :type integer)
  (current 0 :type integer))

(defstruct word
  (name nil :type string)
  (enabledp t :type boolean)
  (immediatep nil :type boolean)
  (code nil :type list))

(defparameter *scanner* (make-scanner))
(defparameter *dictionary* nil)
(defparameter *stack* (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
(defparameter *memory* (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
(defparameter *word-being-compiled* nil)

(defun lookup-word (word)
  (find word *dictionary* :key #'word-name :test #'string=))

(defmacro if-let ((var val) then &optional else)
  `(let ((,var ,val))
     (if ,var ,then ,else)))

(defmacro defword (name (&key (immediatep nil) (enabledp t)) &body body)
  `(if-let (entry (lookup-word ,name))
     (progn (setf (word-code entry) ',body
                  (word-enabledp entry) ,enabledp
                  (word-immediatep entry) ,immediatep)
            (format t "WARNING: REDEFINING '~a'~%" ',name))
     (push (make-word :name ,name :immediatep ,immediatep :enabledp ,enabledp :code ',body) *dictionary*)))

(defmacro defword-forth (&body body)
  `(let ((*scanner* (make-scanner)))
     (with-scanner
       (dolist (line ',body)
         (setf source line
               start 0
               current 0)
         (interprete)))))

(defmacro with-scanner (&body body)
  `(with-slots (source start current) *scanner*
     ,@body))

(defmacro from-top (vector &optional (amount 0))
  `(aref ,vector (- (fill-pointer *stack*) 1 ,amount)))

(defmacro nappend (target &rest lists)
  `(setf ,target (append ,target ,@lists)))

(defun vector-clear (vector)
  (loop repeat (fill-pointer vector)
        do (vector-pop vector)))

(defun vector-insert (new-element index vector)
  (when (>= index (fill-pointer vector))
    (error "Index too big"))
  (vector-push-extend (from-top vector) vector)
  (loop for x from (- (fill-pointer vector) 2) downto (1+ index)
        do (setf (aref vector x) (aref vector (1- x)))
        finally (return (setf (aref vector index) new-element))))

(defun at-end ()
  (with-scanner
    (>= current (length source))))

(defun at-whitespace ()
  (with-scanner
    (sb-unicode:whitespace-p (char source current))))

(defun skip-whitespace ()
  (with-scanner
    (loop while (and (not (at-end))
                     (at-whitespace))
          do (incf current)
          finally (setf start current))))

(defun next-word ()
  (with-scanner
    (loop initially (skip-whitespace)
          until (or (at-end) (at-whitespace))
          do (incf current)
          finally (return (when (/= start current)
                            (subseq source start current))))))

(defun identify-word (word)
  (if-let (entry (lookup-word word))
    (if (equal (word-name entry) *word-being-compiled*)
        (error "Cannot use '~a' while compiling it" word)
        entry)
    (if (every #'digit-char-p word)
        (make-word :name "" :code `((vector-push-extend ,(read-from-string word) *stack*)))
        nil)))

(defun execute-word (code)
  (eval `(progn ,@code)))

(defun compile-word (entry)
  (nappend (word-code (lookup-word *word-being-compiled*))
           (with-slots (name code) entry
             ;; empty name implies this is a number
             (if (string= name "")
                 code
                 `((execute-word (word-code (lookup-word ,name))))))))

(defun interprete ()
  (loop for word = (next-word)
        while word
        do (if-let (entry (identify-word word))
             (with-slots (immediatep code) entry
               (if (and *word-being-compiled* (not immediatep))
                   (compile-word entry)
                   (execute-word code)))
             (error "Cannot find '~a' in dictionary" word))))

(defun forth-repl ()
  (let ((*scanner* (make-scanner))
        (*dictionary* *dictionary*)
        (*stack* (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
        (*memory* (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
        (*word-being-compiled* nil))
    (loop for line = (read-line)
          until (string= line "bye")
          do (with-scanner
               (setf source line
                     start 0
                     current 0)
               (interprete)
               (format t "~t~a~%" *stack*)))))

(defword "lit" ()
  (vector-push-extend (next-word)))

(defword "+" ()
  (vector-push-extend (+ (vector-pop *stack*) (vector-pop *stack*)) *stack*))

(defword "1+" ()
  (incf (from-top *stack*)))

(defword "4+" ()
  (incf (from-top *stack*) 4))

(defword "-" ()
  (let ((right (vector-pop *stack*))
        (left (vector-pop *stack*)))
    (vector-push-extend (- left right) *stack*)))

(defword "1-" ()
  (decf (from-top *stack*)))

(defword "4-" ()
  (decf (from-top *stack*) 4))

(defword "*" ()
  (vector-push-extend (* (vector-pop *stack*) (vector-pop *stack*)) *stack*))

(defword "/mod" ()
  (let ((denominator (vector-pop *stack*))
        (numerator (vector-pop *stack*)))
    (multiple-value-bind (quotient remainder) (floor numerator denominator)
      (vector-push-extend remainder *stack*)
      (vector-push-extend quotient *stack*))))

(defword "=" ()
    (let ((b (vector-pop *stack*))
          (a (vector-pop *stack*)))
      (vector-push-extend (if (= a b) 1 0) *stack*)))

(defword "/=" ()
    (let ((b (vector-pop *stack*))
          (a (vector-pop *stack*)))
      (vector-push-extend (if (/= a b) 1 0) *stack*)))

(defword "0=" ()
  (vector-push-extend (if (zerop (vector-pop *stack*)) 1 0) *stack*))

(defword "0/=" ()
  (vector-push-extend (if (not (zerop (vector-pop *stack*))) 1 0) *stack*))

(defword "<" ()
  (let ((b (vector-pop *stack*))
        (a (vector-pop *stack*)))
    (vector-push-extend (if (< a b) 1 0) *stack*)))

(defword ">" ()
    (let ((b (vector-pop *stack*))
          (a (vector-pop *stack*)))
      (vector-push-extend (if (> a b) 1 0) *stack*)))

(defword "<=" ()
    (let ((b (vector-pop *stack*))
          (a (vector-pop *stack*)))
      (vector-push-extend (if (<= a b) 1 0) *stack*)))

(defword ">=" ()
    (let ((b (vector-pop *stack*))
          (a (vector-pop *stack*)))
      (vector-push-extend (if (>= a b) 1 0) *stack*)))

(defword "0<" ()
  (vector-push-extend (if (< (vector-pop *stack*) 0) 1 0) *stack*))

(defword "0>" ()
  (vector-push-extend (if (> (vector-pop *stack*) 0) 1 0) *stack*))

(defword "0<=" ()
  (vector-push-extend (if (<= (vector-pop *stack*) 0) 1 0) *stack*))

(defword "0>=" ()
  (vector-push-extend (if (>= (vector-pop *stack*) 0) 1 0) *stack*))

(defword "and" ()
    (let ((b (vector-pop *stack*))
          (a (vector-pop *stack*)))
      (vector-push-extend (logand a b) *stack*)))

(defword "or" ()
    (let ((b (vector-pop *stack*))
          (a (vector-pop *stack*)))
      (vector-push-extend (logior a b) *stack*)))

(defword "xor" ()
    (let ((b (vector-pop *stack*))
          (a (vector-pop *stack*)))
      (vector-push-extend (logxor a b) *stack*)))

(defword "not" ()
  (vector-push-extend (lognot (vector-pop *stack*)) *stack*))

(defword "swap" ()
  (rotatef (from-top *stack*) (from-top *stack* 1)))

(defword "2swap" ()
  (rotatef (from-top *stack*) (from-top *stack* 2))
  (rotatef (from-top *stack* 1) (from-top *stack* 3)))

(defword "negate" ()
  (vector-push (- (vector-pop *stack*)) *stack*))

(defword "." ()
  (format t "~a " (vector-pop *stack*)))

(defword "drop" ()
  (vector-pop *stack*))

(defword "2drop" ()
  (vector-pop *stack*)
  (vector-pop *stack*))

(defword "nip" ()
  (setf (from-top *stack* 1) (from-top *stack*))
  (vector-pop *stack*))

(defword "dup" ()
  (vector-push-extend (from-top *stack*) *stack*))

(defword "2dup" ()
  (let ((a (from-top *stack*))
        (b (from-top *stack* 1)))
    (vector-push-extend b *stack*)
    (vector-push-extend a *stack*)))

(defword "?dup" ()
  (unless (zerop (from-top *stack*))
    (vector-push-extend (from-top *stack*) *stack*)))

(defword "over" ()
  (vector-push-extend (from-top *stack* 1) *stack*))

(defword "tuck" ()
  (vector-insert (from-top *stack*) (- (fill-pointer *stack*) 2) *stack*))

(defword "rot" ()
  (rotatef (from-top *stack*) (from-top *stack* 1))
  (rotatef (from-top *stack*) (from-top *stack* 2)))

(defword "-rot" ()
  (rotatef (from-top *stack*) (from-top *stack* 2))
  (rotatef (from-top *stack*) (from-top *stack* 1)))

(defword "\\" (:immediatep t)
  (with-scanner
    (loop until (or (at-end)
                    (char= #\newline (char source current)))
          do (incf current)
          finally (setf start current))))

(defword "(" (:immediatep t)
  (with-scanner
    (loop until (or (at-end)
                    (char= #\) (char source current)))
          do (incf current)
          finally (setf start (incf current)))))

(defword "see" ()
  (format t "~a" (lookup-word (next-word))))

(defword ":" (:immediatep t)
  (if-let (name (next-word))
    (progn (setf *word-being-compiled* name)
           (eval `(defword ,name (:enabledp nil))))
    (error "Cannot make a word with no name")))

(defword ";" (:immediatep t)
  (setf *word-being-compiled* nil
        (word-enabledp (first *dictionary*)) t))

(defword "variable" ()
  (if-let (name (next-word))
    (eval `(defword ,name ()
             (vector-push-extend ,(fill-pointer *memory*) *stack*)))
    (error "Cannot make a word with no name"))
  (vector-push-extend 0 *memory*))

(defword "@" ()
  (vector-push-extend (aref *memory* (vector-pop *stack*)) *stack*))

(defword "!" ()
  (setf (aref *memory* (vector-pop *stack*)) (vector-pop *stack*)))

(defword "+!" ()
  (incf (aref *memory* (vector-pop *stack*)) (vector-pop *stack*)))

(defword "-!" ()
  (decf (aref *memory* (vector-pop *stack*)) (vector-pop *stack*)))

(defword "here" ()
    (vector-push-extend (fill-pointer *memory*) *stack*))

(defword "," ()
  (vector-push-extend (vector-pop *stack*) *memory*))

(defword "allot" ()
  (let ((new-size (+ (fill-pointer *memory*) (vector-pop *stack*))))
    (adjust-array *memory* new-size)
    (setf (fill-pointer *memory*) new-size)))

(defword "cell+" ()
  (incf (from-top *stack*)))

(defword "immediate" ()
  (setf (word-immediatep (first *dictionary*)) t))

(defword-forth ": cells 1 * ;")
