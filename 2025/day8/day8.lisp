(ql:quickload "cl-ppcre")


(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (mapcar #'parse-integer (cl-ppcre:split "," riv ))
  )))

(defun generate-pairs (coords)
      (mapcon #'(lambda (x) (mapcar #'(lambda (y) (list (car x) y)) (cdr x))) coords)
      )

(defun distance (pair)
      (reduce #'+
       (mapcar #'(lambda (x) (* x x)) 
            (mapcar #'- (first pair) (second pair))
      )
      ))

(defun calc-distances (pairs)
      (mapcar #'(lambda (x) (list (distance x) x)) pairs)
)

(defun sort-pairs (pairs)
      (mapcar #'second (sort (calc-distances pairs) #'< :key #'first ) ))

(defun problem1 (path n) 
      (loop
       :with pairs := (sort-pairs (generate-pairs (read-input path)))
       :with circuits := nil
       :with x := 0
       :for pair :in pairs
       ;:do (print (list "pair:" pair "circuits:" circuits))
       ;:when (print (every #'identity (mapcar #'(lambda (x) (set-difference pair x)) circuits)))
       :do (identity (list (incf x) pair (setf circuits (loop
        :for circuit :in circuits
        :if (intersection circuit pair)
        :collect circuit :into matched
        :else 
        :collect circuit :into unmatched
        :finally (return  (cons (reduce #'union (cons pair matched)) unmatched))
        ))))
       :until (= x n)
       :finally (return (apply #'* (subseq (sort (mapcar #'length circuits) #'>) 0 3)) )
       )       
      )


(problem1 "2025/day8/test_input" 10)
(problem1 "2025/day8/input" 1000)

(defun problem2 (path) 
      (loop
       :with pairs := (sort-pairs (generate-pairs (read-input path)))
       :with circuits := nil
       :with last-pair := nil
       :for pair :in pairs
       ;:do (print (list "pair:" pair "circuits:" circuits))
       :when (identity (every #'identity (mapcar #'(lambda (x) (set-difference pair x)) circuits)))
       :do (identity (list (setf last-pair pair) (setf circuits (loop
        :for circuit :in circuits
        :if (intersection circuit pair)
        :collect circuit :into matched
        :else 
        :collect circuit :into unmatched
        :finally (return  (cons (reduce #'union (cons pair matched)) unmatched))
        ))))       
       :finally (return (apply #'* (mapcar #'first last-pair)))
       )       
      )

(problem2 "2025/day8/test_input")

(problem2 "2025/day8/input")