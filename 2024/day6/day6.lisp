;;(ql:quickload "lla")

(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (str-to-list riv) )))



(defun load-map (file)
(list-to-2d-array (read-input file)))

(defun find-init (mapa)
  (loop named outer
    :for x :from 0 :below  (array-dimension  mapa 0)
    :until (loop
         :for y :from 0 :below (array-dimension mapa 1)
         :when (eq (aref mapa x y)  #\^ )
         :do (return-from outer (list x y))
         )
   )
)
   
(defun rot-left(n l)
  (append (nthcdr n l) (butlast l (- (length l) n))))

(defun count-X (mapa)
  (loop  :for x :from 0 :below  (array-dimension  mapa 0)
    :sum (loop
         :for y :from 0 :below (array-dimension mapa 1)
         :count (eq (aref mapa x y)  #\X )         
         )
  ))

(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))


(defun solve-map (mapa initial-coords limit)
  (loop
      :with mapa := (copy-array mapa)
      :with direction := '((-1 0) (0 1) (1 0) (0 -1)) 
      :for iteration :from 1 :to limit
      :for coord := initial-coords :then next-coord
      :for next-coord := (
                       loop 
                        :for next-candidate := (mapcar #'+ coord (car direction))
                        :for next-tile := (ignore-errors (aref mapa (first next-candidate) (second next-candidate)))
                        :until (not (eq next-tile #\#))
                        :do (setq direction (rot-left 1 direction))
                        :finally (return (and next-tile next-candidate)))                                                                                                               
      :do (setf (aref mapa (first coord) (second coord)) #\X)
      :while next-coord
      :finally (return (and (< iteration limit) mapa))
     )  )

(defun solve-first (file)
  (let* (
         (mapa (load-map file))
         (initial-coords (find-init mapa))         
         )
    (count-X (solve-map mapa initial-coords 10000))
    ))



(defun obstruct (mapa x y)
    (let ((new-map (copy-array mapa)))
      (setf (aref new-map x y) #\#)
      new-map
          ))

(defun solve-second (file)
  (let* (
         (mapa (load-map file))
         (initial-coords (find-init mapa))         
         (solved-mapa (solve-map mapa initial-coords 10000))
         (base-steps (count-x  solved-mapa))
         )
    (loop  :for x :from 0 :below  (array-dimension  solved-mapa 0)
      :sum (loop
         :for y :from 0 :below (array-dimension solved-mapa 1)
         :when (and (not (equalp initial-coords (list x y))) (eq (aref solved-mapa x y)  #\X ))
         :count (not (solve-map (obstruct mapa x y) initial-coords (* 10 base-steps)))
         )
    )    
    ))


(solve-first "2024/day6/test_input")
(solve-second "2024/day6/test_input")
(solve-first "2024/day6/input")
(solve-second "2024/day6/input")

; (defun read-input (file)
;   (with-open-file (in file)
;   (loop 
;                   :with iv := nil
;                   :with state := 'top                         
;                   :for riv = (read-line in nil)                   
;                   :while riv                  
;                   :for parsedline = (loop                       
;                     :with ov := t
;                     :for pos := 0 :then (1+ pos)
;                     :while (if ov (ignore-errors (multiple-value-setq (ov pos) (parse-integer riv :start pos :junk-allowed t))))
;                     :collect ov                                    
;                   ) 
;                   :when (eq state 'bottom)
;                       :collect parsedline into manuals
;                   :when (eq state 'top)
;                       :if parsedline
;                       :collect parsedline into rules
;                       :else
;                       :do (setq state 'bottom)                  
;                   :finally (return   (list rules manuals))
     
;      )))

; (read-input "2024/day5/test_input")


; (defvar *test-m2* (make-array '(4 4) :element-type 'boolean :initial-contents '((1 1 1 0) (0 1 0 1) (0 1 1 0) (0 0 0 1))))


; (defun create-identity (size)
;   (loop
;      :with matrix := (make-array (list size size) :element-type 'integer)
;      :for x :from 0 :below size
;      :do (setf (aref matrix x x) 1)
;      :finally (return matrix)
;   )
;   )

; (defun create-connectivity (rules)
;     (loop
;      :with maxpages :=  (1+ (loop :for rule :in rules :maximize (apply #'max rule)))
;      :with connectivity := (create-identity maxpages)
;      :for rule in rules
;      :do (setf (aref connectivity (first rule) (second rule)) 1)
;      :finally (return connectivity)
;     )    
; )

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

; (defun solve-first (filename)
; (destructuring-bind (rules manuals) (read-input filename)    
;     (let 
;         ;;((connectivity (find-all-paths (create-connectivity rules))))
;         ((connectivity (create-connectivity rules)))
;     (loop :for manual in manuals
;           :when (every #'identity ( mapcar (lambda (x y) (> (aref connectivity x y) 0)) manual (cdr manual)))
;           :sum (nth (floor (length manual) 2) manual)
;           )
;     )  ))



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
;             (incf (aref result i j) (min (* (aref a i k) (aref b k j)) 1)))))
;       result)))



; (defvar *test-m* #2A((1 1 1 0) (0 1 0 1) (0 1 1 0) (0 0 0 1)))

; *test-m2*
; (matrix-multiply *test-m2* *test-m2*)


; (defun solve-second (filename)
; (destructuring-bind (rules manuals) (read-input filename)    
;     (let* 
;         ;;((connectivity (find-all-paths (create-connectivity rules))))
;         ((connectivity (create-connectivity rules))
;          (predicate  (lambda (x y) (> (aref connectivity x y) 0)))        
;         )          
;     (loop :for manual in manuals
;           :when (notevery #'identity ( mapcar predicate manual (cdr manual)))                 
;           :sum (nth (floor (length manual) 2) (sort manual predicate))
;           )
;     )  ))



; (solve-first "2024/day5/test_input")
; (solve-second "2024/day5/test_input")
; (solve-first "2024/day5/input")
; (solve-second "2024/day5/input")