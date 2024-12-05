(ql:quickload "lla")

(defun read-input (file)
  (with-open-file (in file)
  (loop 
                  :with iv := nil
                  :with state := 'top                         
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :for parsedline = (loop                       
                    :with ov := t
                    :for pos := 0 :then (1+ pos)
                    :while (if ov (ignore-errors (multiple-value-setq (ov pos) (parse-integer riv :start pos :junk-allowed t))))
                    :collect ov                                    
                  ) 
                  :when (eq state 'bottom)
                      :collect parsedline into manuals
                  :when (eq state 'top)
                      :if parsedline
                      :collect parsedline into rules
                      :else
                      :do (setq state 'bottom)                  
                  :finally (return   (list rules manuals))
     
     )))

(read-input "2024/day5/test_input")


(defvar *test-m2* (make-array '(4 4) :element-type 'boolean :initial-contents '((1 1 1 0) (0 1 0 1) (0 1 1 0) (0 0 0 1))))


(defun create-identity (size)
  (loop
     :with matrix := (make-array (list size size) :element-type 'integer)
     :for x :from 0 :below size
     :do (setf (aref matrix x x) 1)
     :finally (return matrix)
  )
  )

(defun create-connectivity (rules)
    (loop
     :with maxpages :=  (1+ (loop :for rule :in rules :maximize (apply #'max rule)))
     :with connectivity := (create-identity maxpages)
     :for rule in rules
     :do (setf (aref connectivity (first rule) (second rule)) 1)
     :finally (return connectivity)
    )    
)

(defun find-all-paths (connectivity)
  (loop
   :for old_connectivity := nil :then new_connectivity
   :for new_connectivity := connectivity :then (matrix-multiply new_connectivity new_connectivity)
   :for x :from 0
   :always (< x (array-dimension connectivity 0))
   :until (equalp old_connectivity new_connectivity)
   :finally (return new_connectivity)   
   )
)

(defun solve-first (filename)
(destructuring-bind (rules manuals) (read-input filename)    
    (let ((connectivity (find-all-paths (create-connectivity rules))))
    (loop :for manual in manuals
          :when (every #'identity ( mapcar (lambda (x y) (> (aref connectivity x y) 0)) manual (cdr manual)))
          :sum (nth (floor (length manual) 2) manual)
          )
    )  ))

(solve-first "2024/day5/test_input")
(solve-first "2024/day5/input")

(defun matrix-multiply (a b)
  (let* ((rows-a (array-dimension a 0))
         (cols-a (array-dimension a 1))
         (rows-b (array-dimension b 0))
         (cols-b (array-dimension b 1)))
    (assert (= cols-a rows-b) (a b) "Incompatible matrix dimensions for multiplication.")
    (let ((result (make-array (list rows-a cols-b) :initial-element 0)))
      (loop for i below rows-a do
        (loop for j below cols-b do
          (loop for k below cols-a do
            (incf (aref result i j) (min (* (aref a i k) (aref b k j)) 1)))))
      result)))



(defvar *test-m* #2A((1 1 1 0) (0 1 0 1) (0 1 1 0) (0 0 0 1)))

*test-m2*
(matrix-multiply *test-m2* *test-m2*)