(ql:quickload "fiveam")



(defun to-charlist (s) (coerce s 'list))

(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        collect (mapcar (lambda (c) (digit-char-p c)) (to-charlist riv)))))
        

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))



(defmacro pair-ref (arr idx)
    `(apply #'aref (cons ,arr ,idx)))

(defun hides (other-tree my-tree)
    (> other-tree my-tree))


(defun reduce-or (l) 
    (when l (reduce (lambda (x y) (or x y)) l )))



(defun detect-hidden (arr row col)      
    (let ((hf (lambda (x) (>= x (aref arr row col) ))))
     (destructuring-bind (rows cols) (array-dimensions arr)        
            (and
              (reduce-or (mapcar hf (loop for i from 0 below row :collect (aref arr i col))))
              (reduce-or (mapcar hf (loop for i from (1+ row) below rows :collect (aref arr i col))))
              (reduce-or (mapcar hf (loop for i from 0 below col :collect (aref arr row i))))
              (reduce-or (mapcar hf (loop for i from (1+ col) below cols :collect (aref arr row i))))
              ))))


(defun count-tree (tree-line)        
        (loop for tree in tree-line               
              :sum 1 :into acc
              :when tree :do (return acc)              
              :finally (return acc)))


(defun calc-scenic-view (arr row col)      
    (let ((hf (lambda (x) (>= x (aref arr row col) ))))
     (destructuring-bind (rows cols) (array-dimensions arr)            
            (*
              (count-tree  (mapcar hf (reverse (loop for i from 0 below row :collect (aref arr i col)))))
              (count-tree  (mapcar hf (loop for i from (1+ row) below rows :collect (aref arr i col))))
              (count-tree  (mapcar hf (reverse (loop for i from 0 below col :collect (aref arr row i)))))
              (count-tree  (mapcar hf (loop for i from (1+ col) below cols :collect  (aref arr row i))))
              ))))


(defun process-array (arr) 
    (destructuring-bind (rows cols) (array-dimensions arr)    
        (loop
            :for row :from 0 :below rows
            :sum (loop :for col :from 0 :below cols
                        :when (not (detect-hidden arr row col)) :sum 1
                        ))))


(defun process-array-second (arr) 
    (destructuring-bind (rows cols) (array-dimensions arr)    
        (apply 'max (loop :for row :from 0 :below rows
            :collect (apply 'max (loop :for col :from 0 :below cols :collect (calc-scenic-view arr row col)))
         ))))

;(array-total-size (list-to-2d-array  (read-input "day8/test")))

(defvar *test-forest* (list-to-2d-array  (read-input "day8/test")))

(fiveam:def-suite 8am-suite)
(fiveam:in-suite 8am-suite)

(fiveam:test test-hiden
    (fiveam:is  (equal t t))
    )


(DETECT-HIDDEN *TEST-FOREST* 2 2)

(fiveam:test test-forest
    (fiveam:is-false (detect-hidden *test-forest* 0 0))
    (fiveam:is-false (detect-hidden *test-forest* 1 1)) 
    (fiveam:is-false (detect-hidden *test-forest* 1 2))
    (fiveam:is-true  (detect-hidden *test-forest* 1 3))
    (fiveam:is-false (detect-hidden *test-forest* 2 1))
    (fiveam:is-true  (detect-hidden *test-forest* 2 2))
    (fiveam:is-false (detect-hidden *test-forest* 2 3))
    (fiveam:is-true  (detect-hidden *test-forest* 3 1))
    (fiveam:is-false (detect-hidden *test-forest* 3 2))
    (fiveam:is-true  (detect-hidden *test-forest* 3 3))

    (fiveam:is (equal 21 (process-array (list-to-2d-array  (read-input "day8/test")))))
    (fiveam:is (equal 1854 (process-array (list-to-2d-array  (read-input "day8/input")))))
    
    (fiveam:is (equal 4 (calc-scenic-view *test-forest* 1 2)))
    (fiveam:is (equal 8 (calc-scenic-view *test-forest* 3 2)))

    (fiveam:is (equal 8 (process-array-second (list-to-2d-array  (read-input "day8/test")))))
    (fiveam:is (equal 527340 (process-array-second (list-to-2d-array  (read-input "day8/input")))))
    )

(process-array-second *test-forest*)


(fiveam:run! '8am-suite)

