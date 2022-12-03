(ql:quickload "fiveam")

(with-open-file (stream "day1/test_input")
  (format t "~a~%" (read-line stream)))

(defun read-input (file)
  (with-open-file (in file)    
    (loop  :while (listen in)      
      collect (loop 
                   :with iv := nil
                   :for riv = (read-line in nil)
                   :while (if riv (setq iv (parse-integer riv :junk-allowed t)))
                   :collect iv))))

(defun subsum (in)
  (mapcar (lambda (a) (apply '+ a)) in ))

(defun find-max (input-file) (apply 'max (subsum (read-input input-file))))


(defun find-top-three (input-file) 
  (loop  for item in (sort (subsum (read-input input-file)) '>)  for i from 1 to 3  sum item))



(fiveam:test load-successfully
  (fiveam:is (equal 
              '((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000))
              (read-input "day1/test_input"))))

(fiveam:test find-max-input
  (fiveam:is (equal 24000              
              (find-max "day1/test_input"))))

(fiveam:test find-solution
  (fiveam:is (equal 71502              
              (find-max "day1/input"))))


(fiveam:test find-top-three-input
  (fiveam:is (equal 45000              
              (find-top-three "day1/test_input"))))

(fiveam:test find-top-three-solution
  (fiveam:is (equal 208191              
              (find-top-three "day1/input"))))
(fiveam:run!)

