
(ql:quickload "cl-ppcre")

; (defun str-to-list (str)
; (loop :for c :across str :collect c))

; (defun list-to-2d-array (list)
;   (make-array (list (length list)
;                     (length (first list)))
;               :initial-contents list))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (mapcar #'parse-integer (cl-ppcre:split "(:?\\s+)" riv )))))


(defun combinations (a b)
  (if b 
      (combinations (append 
                      (mapcar (lambda (x) (+ x (car b))) a)
                      (mapcar (lambda (x) (* x (car b))) a)



                      )  (cdr b))
      a
    )
  )


(defun solve-one (test-line)
  (let ((result (car test-line))
        (operands (cdr test-line))      
        )
        (if (some #'identity (mapcar (lambda (x) (= x result)) (combinations (list (car operands)) (cdr operands))))
          result 0)
    )
  )

(defun solve-first (file)
  (reduce #'+ (mapcar #'solve-one (read-input file)))    
  )


(defun combinations2 (a b)
  (if b 
      (combinations2 (append 
                      (mapcar (lambda (x) (+ x (car b))) a)
                      (mapcar (lambda (x) (* x (car b))) a)
                      (mapcar (lambda (x) (parse-integer (format nil "~d~d" x (car b)))) a)
                      )  (cdr b))
      a
    )
  )

(defun solve-one-two (test-line)
  (let ((result (car test-line))
        (operands (cdr test-line))      
        )
        (if (some #'identity (mapcar (lambda (x) (= x result)) (combinations2 (list (car operands)) (cdr operands))))
          result 0)
    )
  )

(defun solve-second (file)
  (reduce #'+ (mapcar #'solve-one-two (read-input file)))    
  )


(solve-first "2024/day7/test_input")

(solve-second "2024/day7/test_input")

(solve-first "2024/day7/input")

(solve-second "2024/day7/input")









; (defun load-map (file)
; (list-to-2d-array (read-input file)))

; (defun find-init (mapa)
;   (loop named outer
;     :for x :from 0 :below  (array-dimension  mapa 0)
;     :until (loop
;          :for y :from 0 :below (array-dimension mapa 1)
;          :when (eq (aref mapa x y)  #\^ )
;          :do (return-from outer (list x y))
;          )
;    )
; )
   
; (defun rot-left(n l)
;   (append (nthcdr n l) (butlast l (- (length l) n))))

; (defun count-X (mapa)
;   (loop  :for x :from 0 :below  (array-dimension  mapa 0)
;     :sum (loop
;          :for y :from 0 :below (array-dimension mapa 1)
;          :count (eq (aref mapa x y)  #\X )         
;          )
;   ))

; (defun copy-array (array &key
;                    (element-type (array-element-type array))
;                    (fill-pointer (and (array-has-fill-pointer-p array)
;                                       (fill-pointer array)))
;                    (adjustable (adjustable-array-p array)))
;   "Returns an undisplaced copy of ARRAY, with same fill-pointer and
; adjustability (if any) as the original, unless overridden by the keyword
; arguments."
;   (let* ((dimensions (array-dimensions array))
;          (new-array (make-array dimensions
;                                 :element-type element-type
;                                 :adjustable adjustable
;                                 :fill-pointer fill-pointer)))
;     (dotimes (i (array-total-size array))
;       (setf (row-major-aref new-array i)
;             (row-major-aref array i)))
;     new-array))


; (defun solve-map (mapa initial-coords limit)
;   (loop
;       :with mapa := (copy-array mapa)
;       :with direction := '((-1 0) (0 1) (1 0) (0 -1)) 
;       :for iteration :from 1 :to limit
;       :for coord := initial-coords :then next-coord
;       :for next-coord := (
;                        loop 
;                         :for next-candidate := (mapcar #'+ coord (car direction))
;                         :for next-tile := (ignore-errors (aref mapa (first next-candidate) (second next-candidate)))
;                         :until (not (eq next-tile #\#))
;                         :do (setq direction (rot-left 1 direction))
;                         :finally (return (and next-tile next-candidate)))                                                                                                               
;       :do (setf (aref mapa (first coord) (second coord)) #\X)
;       :while next-coord
;       :finally (return (and (< iteration limit) mapa))
;      )  )

; (defun solve-first (file)
;   (let* (
;          (mapa (load-map file))
;          (initial-coords (find-init mapa))         
;          )
;     (count-X (solve-map mapa initial-coords 10000))
;     ))



; (defun obstruct (mapa x y)
;     (let ((new-map (copy-array mapa)))
;       (setf (aref new-map x y) #\#)
;       new-map
;           ))

; (defun solve-second (file)
;   (let* (
;          (mapa (load-map file))
;          (initial-coords (find-init mapa))         
;          (solved-mapa (solve-map mapa initial-coords 10000))
;          (base-steps (count-x  solved-mapa))
;          )
;     (loop  :for x :from 0 :below  (array-dimension  solved-mapa 0)
;       :sum (loop
;          :for y :from 0 :below (array-dimension solved-mapa 1)
;          :when (and (not (equalp initial-coords (list x y))) (eq (aref solved-mapa x y)  #\X ))
;          :count (not (solve-map (obstruct mapa x y) initial-coords (* 10 base-steps)))
;          )
;     )    
;     ))


; (solve-first "2024/day6/test_input")
; (solve-second "2024/day6/test_input")
; (solve-first "2024/day6/input")
; (solve-second "2024/day6/input")