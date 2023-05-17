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
(defparameter *locals* nil)
(defparameter *stack* (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
(defparameter *memory* (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
(defparameter *compile-mode-p* nil)

(defun lookup-word (word)
  (find word (append *locals* *dictionary*) :key #'word-name :test #'string=))

(defmacro if-let ((var val) then &optional else)
  `(let ((,var ,val))
     (if ,var ,then ,else)))

(defmacro defword (name (&key (immediatep nil) (enabledp t)) &body body)
  `(if-let (entry (lookup-word ,name))
     (progn (setf (word-code entry) ',body
                  (word-enabledp entry) ',enabledp
                  (word-immediatep entry) ',immediatep)
            (format t "WARNING: REDEFINING '~a'" ',name))
     (push (make-word :name ',name :immediatep ,immediatep :enabledp ,enabledp :code ',body) *dictionary*)))

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
    (with-slots (enabledp code) entry
      (if enabledp
          entry
          (error "Cannot use '~a' while compiling it" word)))
    (if (every #'digit-char-p word)
        (make-word :name "" :code `((vector-push-extend ,(read-from-string word) *stack*)))
        nil)))

(defun execute-word (code)
  (eval `(progn ,@code)))

(defun compile-word (entry)
  ;; the compile body shall always be at the top of the dictionary
  (setf (word-code (first *dictionary*))
        (append (word-code (first *dictionary*))
                (with-slots (name code) entry
                  ;; empty name implies this is a number
                  (if (string= name "")
                      code
                      `((execute-word (word-code (lookup-word ,name)))))))))

(defun interprete ()
  (loop for word = (next-word)
        while word
        do (if-let (entry (identify-word word))
             (with-slots (immediatep code) entry
               (if (and *compile-mode-p* (not immediatep))
                   (compile-word entry)
                   (execute-word code)))
             (error "Cannot find '~a' in dictionary" word))))

(defun forth-repl ()
  (unwind-protect
       (loop for line = (read-line)
             until (string= line "bye")
             do (with-scanner
                  (setf source line
                        start 0
                        current 0)
                  (interprete)
                  (format t "~t~a~%" *stack*)))
    (setf *compile-mode-p* nil
          *locals* nil)
    (vector-clear *stack*)
    (vector-clear *memory*)))

(defword "+" ()
  (vector-push-extend (+ (vector-pop *stack*) (vector-pop *stack*)) *stack*))

(defword "-" ()
  (let ((right (vector-pop *stack*))
        (left (vector-pop *stack*)))
    (vector-push-extend (- left right) *stack*)))

(defword "*" ()
  (vector-push-extend (* (vector-pop *stack*) (vector-pop *stack*)) *stack*))

(defword "/" ()
  (let ((denominator (vector-pop *stack*))
        (numerator (vector-pop *stack*)))
    (vector-push-extend (/ numerator denominator) *stack*)))

(defword "mod" ()
  (let ((divisor (vector-pop *stack*))
        (number (vector-pop *stack*)))
    (vector-push-extend (mod number divisor) *stack*)))

(defword "swap" ()
  (rotatef (from-top *stack*) (from-top *stack* 1)))

(defword "2swap" ()
  (rotatef (from-top *stack*) (from-top *stack* 2))
  (rotatef (from-top *stack* 1) (from-top *stack* 3)))

(defword "/mod" ()
  (let ((denominator (vector-pop *stack*)) (numerator (vector-pop *stack*)))
    (multiple-value-bind (quotient remainder) (floor numerator denominator)
      (vector-push-extend remainder *stack*)
      (vector-push-extend quotient *stack*))))

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

(defword "over" ()
  (vector-push-extend (from-top *stack* 1) *stack*))

(defword "tuck" ()
  (vector-insert (from-top *stack*) (- (fill-pointer *stack*) 2) *stack*))

(defword "rot" ()
  (rotatef (from-top *stack*) (from-top *stack* 1))
  (rotatef (from-top *stack*) (from-top *stack* 2)))

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

(defword "{" (:immediatep t)
  (with-scanner
    (loop for word = (next-word)
          until (or (at-end)
                    (string= word "}"))
          do (push (make-word :name word) *locals*))
    (nappend (word-code (first *dictionary*))
             (loop for word in *locals*
                   collect `(push (make-word :name ,(word-name word)
                                             :code `((vector-push ,(vector-pop *stack*) *stack*)))
                                  *locals*)))))

(defword "see" ()
  (format t "~a" (lookup-word (next-word))))

(defword ":" (:immediatep t)
  (setf *compile-mode-p* t)
  (if-let (name (next-word))
    (eval `(defword ,name (:enabledp nil)))
    (error "Cannot make a word with no name")))

(defword ";" (:immediatep t)
  (nappend (word-code (first *dictionary*)) '((setf *locals* nil)))
  (setf *compile-mode-p* nil
        (word-enabledp (first *dictionary*)) t
        *locals* nil))

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

(defword "immediate" ()
  (setf (word-immediatep (first *dictionary*)) t))
