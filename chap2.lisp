(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (setf *small* 1)
  (setf *big* 100)
  (guess-my-number))

(flet ((f (n)
          (+ n 10))
       (g (n)
          (+ n 10)))
  (print (f (g 5))))

(labels ((a (n)
            (+ n 5))
         (b (n)
            (+ (a n) 6)))
  (print (b 10)))
