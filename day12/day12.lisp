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

(defun has-negative-coord (coord) 
    (reduce (lambda (d e) (or d e))(mapcar #'minusp coord)))

(defun upper-bound-checker (upper-bound) 
    (lambda (coord) (reduce (lambda (d e) (or d e))(mapcar #'>= coord upper-bound))))

(defun out-of-bound-checker (upper-bound)
    (let ((has-above-bound-coord (upper-bound-checker upper-bound)))
     (lambda (coord) (or
                        (has-negative-coord coord)
                        (funcall has-above-bound-coord coord)))))

(defun adjacent-coords (coord bound-checker)
    (remove-if bound-checker
        (mapcar (lambda(b) (mapcar '+ coord b)) '((1 0) (-1 0) (0 1) (0 -1))) ;generate neighbour coords from kernel
        ))

(defmacro pair-ref (arr idx)
    `(apply #'aref (cons ,arr ,idx)))

(defun char-to-weight (char)
    (char-int (cond
     ((char= #\S char) #\a)
     ((char= #\E char) #\z)
     (t char))))

(defun coord-weight-checker (map)
    (lambda (coord) (char-to-weight (pair-ref map coord)) ))

(defun adjacent-validator (weight-checker base-coord)
    (lambda (adj-coord) (>= 
                             (1+ (funcall weight-checker base-coord)) 
                             (funcall weight-checker adj-coord))))

(defun adjacences-finder (map) 
    (lambda (coord) (remove-if-not (adjacent-validator (coord-weight-checker map) coord) (adjacent-coords coord (out-of-bound-checker (array-dimensions map))))))

(defvar *test-map* (load-map "day12/test") )


(defun gen-graph-coord-start (map start-distance)
    (loop with adj-finder = (adjacences-finder map)
     for coord in (gen-coords (array-dimensions map))
        as coord-char = (pair-ref map coord)
        collect (list
                   :coord coord
                   :distance (funcall start-distance coord)
                   :adjacences (funcall adj-finder coord)
                   :goal (char= #\E coord-char)
                   )
              ))

(defun gen-graph (map)
    (gen-graph-coord-start map (lambda (c) (when (char= #\S (pair-ref map c)) 0) )))


(defun mincomp (a b)
     (cond
      ((not a) nil)
      ((not b) t)
      (t (< a b))))

(defun sort-nodes (graph)
    (sort graph 'mincomp :key (lambda (c) (getf c :distance))))

(defun find-path-len (map)
    (do* (
          (graph (sort-nodes (gen-graph map))
                 (sort-nodes (mapcar (lambda (n) 
                                         (when (and
                                                (member (getf n :coord) (getf current :adjacences) :test 'equal)
                                                (mincomp next-distance (getf n :distance)))
                                               (setf (getf n :distance) next-distance))
                                         n)  (cdr graph))))                
          (current (car graph) (car graph))
          (next-distance (1+ (getf current :distance)) (1+ (getf current :distance)))
          )             
            ((getf current :goal) (getf current :distance))
        ))




(fiveam:def-suite 12am-suite)
(fiveam:in-suite 12am-suite)

(fiveam:test test-negative-coord 
    (fiveam:is-true (has-negative-coord '(-1 -1)))
    (fiveam:is-true (has-negative-coord '(-1  1)))
    (fiveam:is-true (has-negative-coord '( 1 -1)))
    (fiveam:is-false (has-negative-coord '( 1  1)))
    (fiveam:is-false (has-negative-coord '( 0  0))))


(fiveam:test test-adjacents
    (fiveam:is (equal '((3 3) (1 3) (2 4) (2 2)) (adjacent-coords '(2 3) (out-of-bound-checker (array-dimensions *test-map*)))))
    (fiveam:is (equal '(t t nil t) (mapcar (adjacent-validator (coord-weight-checker *test-map*) '(2 3)) '((3 3) (1 3) (2 4) (2 2)))))
    (fiveam:is (equal '((3 3) (1 3) (2 2)) (funcall (adjacences-finder *test-map*) '(2 3))))
    )

(fiveam:test test-process
    (fiveam:is (equal 31 (find-path-len *test-map*)))
    (fiveam:is (equal 447 (find-path-len (load-map "day12/input"))))
    )
    

(fiveam:run! '12am-suite)