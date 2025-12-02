(ql:quickload "fiveam")

(defun read-input (file)
  (with-open-file (in file)
  (loop           :for riv = (read-line in nil)                  
                  :while riv                  
                  :collect (list (char riv 0) (parse-integer riv :start 1)))))


(read-input "2025/day1/test_input")


(defun solve (path)
(loop 
      :with pos := 50
      :for iv in (read-input path)
      :for op := (if (eq (first iv) #\R ) '+ '-)
      :when (eq (setf pos (mod (funcall op pos (second iv)) 100 )) 0)
      :count pos )
)

(floor (/ -101 100))

(solve "2025/day1/test_input")
(solve "2025/day1/input")


(defun solve2 (path)
(loop 
      :with pos := 50
      :with q := 0
      :with r := 0
      :with delta := 0
      :for iv in (read-input path)
      :for op := (if (eq (first iv) #\R ) '+ '-)
      :for posold := pos      
      :do (setf pos (if (eq pos 0) (if (eq (first iv) #\R ) 0 100) pos))
      :do (setf (values q r) (floor (funcall op pos (second iv)) 100))      
      :do (setf delta (+ (abs q) (if (and (<= q 0) (eq r 0)) 1 0)))
      :sum delta
      ;:do (print (list posold iv pos q r delta))
      :do (setf pos r)             
)
)

(solve2 "2025/day1/test_input")
(solve2 "2025/day1/test_input2")
(solve2 "2025/day1/input")