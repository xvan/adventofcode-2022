(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (str-to-list riv) )))




(defvar *test_data* (read-input "2024/day4/test_input"))
(defvar *test_data2* (read-input "2024/day4/test_input2"))


(defun rot-left(n l)
  (append (nthcdr n l) (butlast l (- (length l) n))))

(defun rot-right(n l)
  (rot-left (- (length l) n) l))
 

(defun rot-45 (input)
(rotate (loop :with inputT = (rotate input)
      :with padding = ( make-list (- (length inputT) 1) )
      :for line in inputT
      :for n :from 0
      :collect  (rot-right n (append  line padding)
        ))))

(defun rot+45 (input)
(rotate (loop :with inputT = (rotate input)
      :with padding = ( make-list (- (length inputT) 1) )
      :for line in inputT
      :for n :from 0
      :collect  (rot-left n (append  padding line)
        ))))


(find-xmas (rot-45 *test_data*))
;
(defun find-xmas (input)
  (let* ((xmas (str-to-list "XMAS"))
         (samx (reverse xmas)) 
         (tokens (list xmas samx))
         )
    (loop :for rotated-direction :in (list input (rotate input) (rot+45 input) (rot-45 input))     
     :sum ( loop
                  :for line
                  :in rotated-direction
                  :sum ( loop :for token :in tokens
                    :sum ( loop 
                        :for start = 0 :then (+ 1 start)
                        :while (setq start (search token line :start2 start))
                        :count t
                  )))
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(find-xmas2 (list-to-2d-array *test_data2*))


(defun find-xmas2 (input)
  (loop :for x :from 0 :below (array-dimension  input 0) :sum
  (loop :for y :from 0 :below (array-dimension  input 1)
    
    :count (ignore-errors (and 
                          (eq #\A  (aref input x y))
                          (find (list (aref input (1- x) (1- y)) (aref input (1+ x) (1+ y))) '((#\M #\S) (#\S #\M)) :test #'equal)
                          (find (list (aref input (1+ x) (1- y)) (aref input (1- x) (1+ y))) '((#\M #\S) (#\S #\M)) :test #'equal)
                  )))))
     ;;(and (eq #\A  (aref input x y)) ( search (print (list (aref input (1- x) (1- y)) (aref input (1+ x) (1+ y)))) '((#\M #\S) (#\S #\M)) :test #'equal)


(find-xmas2 (list-to-2d-array (read-input "2024/day4/input")))


(loop for tt in (find-xmas2 (list-to-2d-array *test_data2*))  :count ( find tt '((#\M #\S) (#\S #\M)) :test #'equal))


(find-xmas (read-input "2024/day4/test_input"))
(find-xmas (read-input "2024/day4/input"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










