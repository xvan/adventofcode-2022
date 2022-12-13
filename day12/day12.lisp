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


(defun gen-graph (map)
    (gen-coords (array-dimensions map)))

(defun has-negative-coord (b) 
    (reduce (lambda (d e) (or d e))(mapcar #'minusp b)))

(defun adjacent-coords (coord)
    (remove-if 'has-negative-coord              
        (mapcar (lambda(b) (mapcar '+ coord b)) '((1 0) (-1 0) (0 1) (0 -1))) ;generate neighbour coords from kernel
        ))




(has-negative-coord '(-1 -1))

(mapcar (lambda(b) (mapcar '+ '(1 1)) b)) '((1 0) (-1 0) (0 1) (0 -1))

(adjacent-coords '(0 1))
(gen-graph (load-map "day12/test"))


(fiveam:def-suite 12am-suite)
(fiveam:in-suite 12am-suite)

(fiveam:test test-parser     
    (fiveam:is-true t)
    )

(fiveam:test test-process 

    )

(fiveam:run! '12am-suite)