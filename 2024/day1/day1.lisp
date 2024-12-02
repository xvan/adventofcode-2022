(ql:quickload "fiveam")

(with-open-file (stream "2024/day1/test_input")
  (format t "~a~%" (read-line stream)))


(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun read-input (file)
  (rotate
  (with-open-file (in file)
  (loop 
                  :with iv := nil                   
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (loop 
                    :with pos := 0
                    :with ov := 0
                    :while (if ov (multiple-value-setq (ov pos) (parse-integer riv :start pos :junk-allowed t)))
                    :collect ov
                  ) ))))


(defun sort-diff-sum (pair-of-lists)
  (apply #'+ (mapcar #'abs ( apply #'mapcar #'- (mapcar (lambda (x) (sort x #'<)) pair-of-lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-repeats (test-number control-list)
  (count-if (lambda (x) (= x test-number)) control-list))


(defun similarity-score (test-list control-list)
(apply #'+ (mapcar (lambda (test-number) (* test-number (count-repeats test-number control-list))) test-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test load-successfully
  (fiveam:is (equal 
              '((3 4 2 1 3 3) (4 3 5 3 9 3))
              (read-input "2024/day1/test_input"))))

(fiveam:test sort-diff-sum-succesfully
  (fiveam:is (equal 
              11
              (sort-diff-sum '((3 4 2 1 3 3) (4 3 5 3 9 3))))))

(fiveam:test count-repeats-succesfully
  (fiveam:is (equal 
              3
              (count-repeats 3 '(4 3 5 3 9 3)))))              
              
(fiveam:test calculate-similarity-score-succesfully
  (fiveam:is (equal 
              31
              (similarity-score '(3 4 2 1 3 3) '(4 3 5 3 9 3)))))


(fiveam:test calculate-similarity-score-succesfully2
  (fiveam:is (equal 
              31
              (apply #'similarity-score (read-input "2024/day1/test_input")))))

(fiveam:run!)

(sort-diff-sum (read-input "2024/day1/input"))
(apply #'similarity-score (read-input "2024/day1/input"))