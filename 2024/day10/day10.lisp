(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defmacro aref-2d (array indices)
  `(aref ,array (first ,indices) (second ,indices)))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (mapcar (lambda (c) (- (char-code c) (char-code #\0))) (str-to-list riv)) )))

(defun load-map (file)
(list-to-2d-array (read-input file)))

(defun get-coords (topo-map)
  (sort 
  (loop :for x :from 0 :below (array-dimension topo-map 0)
    :append (loop :for y :from 0 :below (array-dimension topo-map 1)
                :when (<= 0 (aref topo-map x y) 9)
                :collect (list x y )))      
    (lambda (a b) (> (aref-2d topo-map a) (aref-2d topo-map b)))))


(defun make-score-map (topo-map sorted-coords)
  (loop
    :with score-map := (make-array (array-dimensions topo-map) :initial-element '())
    :for c :in sorted-coords
    :for id :from 0
    :while (= 9 (aref-2d topo-map c))      
    :do (push id (aref-2d score-map c))
    :finally (return score-map)))


(defun validate-coord (coord dimensions)
  (every #'identity (mapcar (lambda (x y) (and (>= x 0) (< x y))) coord dimensions)))

(defun get-neighbours (c dimensions)
  (remove-if-not (lambda (n) (validate-coord n dimensions)) (mapcar (lambda (d) (mapcar #'+ c d)) '((-1 0) (0 1) (1 0) (0 -1)))))


(defmacro collect-all (source target collect-fn)
  `(dolist (item ,source ,target)
     (setf ,target (funcall ,collect-fn item ,target))))

(defun solve-both (topo-map collect-fn)  
  (loop
   :with sorted-coords := (get-coords topo-map)
   :with score-map := (make-score-map topo-map sorted-coords)
   :with dimensions := (array-dimensions topo-map)
   :for c :in sorted-coords
   :for h := (aref-2d topo-map  c)
   :for s :=  (aref-2d score-map c)
   :do (
      loop :for n :in (get-neighbours c dimensions)
             :when (= h (1+ (aref-2d topo-map n)))             
             :do (collect-all s (aref-2d score-map n) collect-fn)
             )
   :when (= h 0)
   :sum (length s)   
  ))


(defun solve (file n)
  (let ((topo-map (load-map file)))
    (cond 
     ((= n 1) (solve-both topo-map #'adjoin))
     ((= n 2)  (solve-both topo-map #'cons))
    )))

;(untrace next-empty-range)
;(trace find-empty-range)
;(untrace next-empty-range)
;(untrace next-file)


(solve "2024/day10/test_input0" 1)
(solve "2024/day10/test_input1" 1)
(solve "2024/day10/test_input2" 1)
(solve "2024/day10/test_input" 1)
(solve "2024/day10/input" 1)

(solve "2024/day10/test_input" 2)
(solve "2024/day10/input" 2)



(defun my-adjoin (source target)
  (dolist (item source target)
    (setf target (adjoin item target))))

(let ((kk '(1 2 3 4 5))) (my-adjoin '(1 2 7) kk) kk )

; (defun map-to-graph (map-c dimensions)
;   (+(* (first map-c) (first dimensions)) (second map-c))
;   )



; (defun make-graph (topo-map)
;   (let* (
;         (dimensions (array-dimensions topo-map))
;         (elements  (apply #'* dimensions))
;         (graph (make-array  (list elements elements) :element-type 'integer))
;         (sorted-coords (get-coords topo-map))     
;         )
;     (loop :for map-c :in sorted-coords 
;           :for graph-c := (map-to-graph map-c dimensions)
;             :do (incf (aref graph graph-c graph-c)))
    
;     (loop :for map-c :in sorted-coords 
;           :for h := (aref-2d topo-map map-c)
;           :for graph-c := (map-to-graph map-c dimensions)
;             :do (
;                loop :for map-n :in (get-neighbours map-c dimensions)
;                       :when (= h (1+ (aref-2d topo-map map-n)))
;                       :do (incf (aref graph graph-c (map-to-graph map-n dimensions))))
;     )
;     graph
;   ))


; (defun matrix-multiply (a b)
;   (let* ((rows-a (array-dimension a 0))
;          (cols-a (array-dimension a 1))
;          (rows-b (array-dimension b 0))
;          (cols-b (array-dimension b 1)))
;     (assert (= cols-a rows-b) (a b) "Incompatible matrix dimensions for multiplication.")
;     (let ((result (make-array (list rows-a cols-b) :initial-element 0)))
;       (loop for i below rows-a do
;         (loop for j below cols-b do
;           (loop for k below cols-a do
;             (when (= 0 (aref result i j))(setf (aref result i j) (min (* (aref a i k) (aref b k j)) 1))))))
;       result)))


; (defun find-all-paths (connectivity)
;   (loop
;    :for old_connectivity := nil :then new_connectivity
;    :for new_connectivity := connectivity :then (matrix-multiply new_connectivity new_connectivity)
;    :for x :from 0
;    :always (< x (array-dimension connectivity 0))
;    :until (equalp old_connectivity new_connectivity)
;    :finally (return new_connectivity)   
;    )
; )

; (defun solve-first-bad (topo-map)
;   (loop
;    with dimensions := (array-dimensions topo-map)
;    with graph := (find-all-paths (make-graph topo-map))
;    with (nines zeros) := (
;                         loop :for c in (get-coords topo-map)
;                              :for v := (aref-2d topo-map c)
;                              :if (= v 0) :collect c :into zeros
;                              :if (= v 9) :collect c :into nines
;                              :finally (return (list nines zeros))
;                                )
;    :for nine-graph :in (mapcar (lambda (x) (map-to-graph x dimensions)) nines)   
;     :sum (loop :for zero-graph :in (mapcar (lambda (x) (map-to-graph x dimensions)) zeros)   
;           :sum (aref graph nine-graph zero-graph)
;     )
;   ))


