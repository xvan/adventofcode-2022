(ql:quickload "fiveam")



(defun generate-keys () )

(fiveam:test generate-combinations
  (fiveam:is (= 6 (length (generate-keys)))))
(fiveam:run!)