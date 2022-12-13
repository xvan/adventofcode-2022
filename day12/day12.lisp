(ql:quickload "fiveam")

(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :collect riv )))

(defun to-charlist (s) (coerce s 'list))

(defun load-map (file)
    (list-to-2d-array  (loop for line in (read-input file) collect (to-charlist line))))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun gen-coords (dimensions)
    (if dimensions 
        (loop
        with tail = (gen-coords (cdr dimensions))
        for n from 0 below (car dimensions)
            append (loop for coord in tail collect (cons n coord))     
        )
        '(())))

(defun has-negative-coord (b) 
    (reduce (lambda (d e) (or d e))(mapcar #'minusp b)))

(defun adjacent-coords (coord)
    (remove-if 'has-negative-coord              
        (mapcar (lambda(b) (mapcar '+ coord b)) '((1 0) (-1 0) (0 1) (0 -1))) ;generate neighbour coords from kernel
        ))

(defmacro pair-ref (arr idx)
    `(apply #'aref (cons ,arr ,idx)))

(defun char-to-weight (char)
    (char-int (cond
     ((char-equal #\S char) #\a)
     ((char-equal #\E char) #\z)
     (t char))))

(defun cood-weight-checker (map)
    (lambda (coord) (char-to-weight (pair-ref map coord)) )

;(adjacent-coords '(0 1))
(defun adjacent-validator (weight-checker base-coord)
    (lambda (adj-coord) (>= 
                             (1+ (funcall weight-checker base-coord)) 
                             (funcall weight-checker base-coord))))


(defun gen-graph (map)
    (gen-coords (array-dimensions map)))

(gen-graph (load-map "day12/test"))

(fiveam:def-suite 12am-suite)
(fiveam:in-suite 12am-suite)

(fiveam:test test-negative-coord 
    (fiveam:is-true (has-negative-coord '(-1 -1)))
    (fiveam:is-true (has-negative-coord '(-1  1)))
    (fiveam:is-true (has-negative-coord '( 1 -1)))
    (fiveam:is-false (has-negative-coord '( 1  1)))
    (fiveam:is-false (has-negative-coord '( 0  0)))
        )

(fiveam:test test-process 

    )

(fiveam:run! '12am-suite)