(ql:quickload "fiveam")

(defun split-by-char (string separator)    
    (loop for i = 0 then (1+ j)
          as j = (position separator string :start i)
          collect (subseq string i j)
          while j))

(defun parse-line (line)
    (mapcar (lambda (l) (mapcar 'parse-integer (split-by-char l #\-))) (split-by-char line #\,)))

(defun read-input (file)
  (with-open-file (in file)    
    (loop   :for riv = (read-line in nil) 
            :while riv
            :collect (parse-line riv))))

(defun expand-range (range)
    (loop for i from (first range) to (second range) collect i))

(defun contained (sublist main)
    (not (loop for item in sublist
        do (when (not (member item main)) (return t)))))

(defun range-overlaps (range-pair)
    (let ((expanded (mapcar 'expand-range range-pair)))
        (or (apply 'contained expanded) (apply 'contained (reverse expanded)))))

(defun count-overlaps (range-pairs)
    (apply #'+ (mapcar (lambda (p) (if (range-overlaps p) 1 0)) range-pairs)))

(defun count-pisada (range-pairs)
    (apply #'+ (mapcar (lambda (r) (if (apply 'intersection (mapcar 'expand-range r)) 1 0)) range-pairs)))
    



(mapcar (lambda (p) (list p (apply 'intersection p))) (read-input "day4/input"))

(fiveam:test test-split-by-char (fiveam:is (equal '("hola" "chau") (split-by-char "hola,chau" #\, ))))
(fiveam:test test-parse-line (fiveam:is (equal '((2 4)(6 8)) (parse-line "2-4,6-8"))))
(fiveam:test test-read-input (fiveam:is (equal '(((2 4)(6 8))((2 3) (4 5))((5 7)(7 9))((2 8)(3 7))((6 6)(4 6))((2 6)(4 8))) (read-input "day4/test"))))
(fiveam:test test-expand-range 
    (fiveam:is (equal '(1 2 3) (expand-range '(1 3))))
    (fiveam:is (equal '(1 2) (expand-range '(1 2))))
    (fiveam:is (equal '(2) (expand-range '(2 2)))))
(fiveam:test test-range-overlaps
    (fiveam:is-true (range-overlaps '((2 8)(3 7))))
    (fiveam:is (null (range-overlaps '((2 4)(6 8))))))
(fiveam:test process-test 
    (fiveam:is (equal 4 (count-pisada (read-input "day4/test"))))
    (fiveam:is (equal 1 (count-pisada (read-input "day4/input")))))

(fiveam:run!)