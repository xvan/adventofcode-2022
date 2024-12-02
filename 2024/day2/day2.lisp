(ql:quickload "fiveam")


; (defconstant test_data 
; '((7 6 4 2 1)
; (1 2 7 8 9)
; (9 7 6 2 1)
; (1 3 2 4 5)
; (8 6 4 4 1)
; (1 3 6 7 9))
; )


(defun read-input (file)
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
                  ) )))


(defun is-safe (test_line)
(let* (( diff_line (mapcar #'- test_line (cdr test_line))        
  ))
    (cond      
      ( (every (lambda (x) (and (< x 4) (> x 0) )) diff_line) t)
      ( (every (lambda (x) (and (> x -4) (< x 0) )) diff_line) t)
    )  
  )
)


(defun boolean-to-integer (value)
  (if value 1 0))

(defun safe-reports (data)
  (apply #'+ (mapcar #'boolean-to-integer (mapcar #'is-safe data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun variations (test_line)
  (loop for idx below (length test_line) collect
          (loop :for i :in test_line 
              :for jdx :from 0
              :unless (= jdx idx) :collect i)
          )  
  )

(defun is-safe-with-tolerance (test_line)
    (some #'identity (mapcar #'is-safe (variations test_line))))

(defun safe-reports-with-tolerance (data)
  (apply #'+ (mapcar #'boolean-to-integer (mapcar #'is-safe-with-tolerance data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test load-successfully
  (fiveam:is (equal 
              test_data
              (read-input "2024/day2/test_input"))))


(fiveam:test check-safe-successfully
  (fiveam:is (equal 
              '(t nil nil nil nil t)              
               (mapcar #'is-safe test_data))))

(fiveam:test sum-safe-successfully
  (fiveam:is (equal 
              2
              (safe-reports test_data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test check-safe-with-tolerance-successfully
  (fiveam:is (equal 
              '(t nil nil t t t)              
               (mapcar #'is-safe-with-tolerance test_data))))


(fiveam:test sum-safe-with-tolerance-successfully
  (fiveam:is (equal 
              4
              (safe-reports-with-tolerance test_data))))

(fiveam:run!)


(safe-reports (read-input "2024/day2/input"))
(safe-reports-with-tolerance (read-input "2024/day2/input"))







