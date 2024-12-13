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


(defmacro aref-2d (array indices)
  `(aref ,array (first ,indices) (second ,indices)))

(defun validate-coord (coord dimensions)
  (every #'identity (mapcar (lambda (x y) (and (>= x 0) (< x y))) coord dimensions)))

(defun get-neighbours (c dimensions)
  (remove-if-not (lambda (n) (validate-coord n dimensions)) (mapcar (lambda (d) (mapcar #'+ c d)) '((-1 0) (0 -1)))))

(defun save-region (region c region-map region-store perimeter-delta)
  (let ((region-record (gethash region region-store '(0 ()))))
    (setf (aref-2d region-map c) region)
    (incf (first region-record) perimeter-delta)
    (push c (second region-record))
    )
    
    )

(defun merge-region (neighbours region-map region-store)
  (let* ((regions (mapcar (lambda (c) (aref-2d region-map c)) neighbours))
         (records (mapcar (lambda (r) (gethash r region-store)) regions)))
    (when (not (= (first regions) (second regions)))
      (incf (first (first records)) (first (second records)))
      (mapc (lambda (x)
              (push x (second (first records)))
              (setf (aref-2d region-map x) (first (first records))))
            (second (second records)))
      (remhash (second regions) region-store))))

(defun solve-first (garden)
  (let* ((dims (array-dimensions (print garden)))
         (region-iter 0)
         (region-map (make-array dims :element-type 'integer))
         (region-store (make-hash-table)))
    (loop 
     :with lenx := (array-dimension garden 0)
     :with leny := (array-dimension garden 1)
     :for x :from 0 :below lenx
     :do (loop
          :for y :from 0 :below leny
          :for c := (list x y)
          :for plant := (aref-2d garden c)        
          :for neighbours := (remove-if-not (lambda (n) (equal plant (aref-2d garden n))) (get-neighbours c (list x y)))
          :do (cond
               ((not neighbours) (save-region (incf region-iter) c region-map region-store 4))
               ((= (length neighbours) 1) (save-region (aref-2d region-map (first neighbours)) c region-map region-store 2))
               ((= (length neighbours) 2) 
                (merge-region neighbours region-map region-store)
                (save-region (aref-2d region-map (first neighbours)) c region-map region-store 0))))
     :finally (return
               (loop :for value :being :the hash-values :of (print region-store)
                     :sum (* (first value) (length (second value))))))))

(defun solve (file n)
  (let ((garden (load-map file)))
    (cond 
     ((= n 1) (solve-first garden))
     ((= n 2) garden)
    )))



;(untrace next-empty-range)
(trace save-region)
;(untrace next-empty-range)
;(untrace next-file)


(solve "2024/day12/test_input0" 1)
(solve "2024/day12/test_input1" 1)
(solve "2024/day12/test_input2" 1)
;(solve "2024/day12/input" 2)
