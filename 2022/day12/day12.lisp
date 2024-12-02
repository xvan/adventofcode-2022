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


(defun mincomp (a b)
     (cond
      ((not a) nil)
      ((not b) t)
      (t (< a b))))

(defun sort-nodes (graph)
    (sort graph 'mincomp :key (lambda (c) (getf c :distance))))

(defun find-path-len (map)
    (find-path-len-coord-start map (lambda (c) (when (char= #\S (pair-ref map c)) 0) )))


(defun find-path-len-coord-start (map start-distance)
    (do* (
          (graph (sort-nodes (gen-graph-coord-start map start-distance))
                 (sort-nodes (mapcar (lambda (n) 
                                         (when (and
                                                (member (getf n :coord) (getf current :adjacences) :test 'equal)
                                                (mincomp  (1+ distance) (getf n :distance)))
                                               (setf (getf n :distance) (1+ distance)))
                                         n)  (cdr graph))))                
          (current (car graph) (car graph))
          (distance (getf current :distance) (getf current :distance))
          )             
            ((getf current :goal) distance)
            (when (not distance) (return))
        ))

(defun find-min-path (map)
    (loop
     with coord-weighter = (coord-weight-checker map) 
     with root-weight = (char-to-weight #\a) 
     for coord in (gen-coords (array-dimensions map))
     ;for c = 0 then ( (print1+ c))
     as path-len = nil
     when (= root-weight (funcall coord-weighter coord)) do (setf path-len (find-path-len-coord-start map (lambda (c) (when (equal coord c) 0) )))
     when path-len minimize path-len
     ))



(defun find-all-paths (map start-distance)
    (do* (
          (graph (sort-nodes (gen-graph-coord-start map start-distance))
                 (sort-nodes (mapcar (lambda (n) 
                                         (when (and
                                                (member (getf n :coord) (getf current :adjacences) :test 'equal)
                                                (mincomp  (1+ distance) (getf n :distance)))
                                               (setf (getf n :distance) (1+ distance)))
                                         n)  (cdr graph))))                
          (current (car graph) (car graph))
          (distance (getf current :distance) (getf current :distance))
          )             
            ((getf current :goal) distance)
            (when (not distance) (return))
        ))



;(find-min-path *test-map*)

;(find-min-path (load-map "day12/input"))

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
    (fiveam:is (equal 29 (find-min-path *test-map*)))
    ;(fiveam:is (equal 446 (find-min-path (load-map "day12/input"))))
    )
    ;

    

(fiveam:run! '12am-suite)