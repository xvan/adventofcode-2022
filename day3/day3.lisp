(ql:quickload "fiveam")

(defun split-string (s) (list s)
    (let ((half (floor (length s) 2)))
         (list (subseq s 0 half) (subseq s half))))

(defun to-charlist (s) (coerce s 'list))

(defun find-repeated (s) 
    (first (apply 'intersection (mapcar 'to-charlist (split-string s)))))

(defun get-score (c) 
    (1+ (position c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )))

(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :sum (get-score (find-repeated riv)))))

(defun read-input-by-3 (file)
  (with-open-file (in file) 
    (loop  :while (listen in)      
    :collect (loop         
        for i from 1 to 3 :for riv = (read-line in nil) 
        :while riv
        :collect riv ))))

(defun find-badge (file) 
    (apply '+ (mapcar (lambda (b) (apply 'get-score (reduce 'intersection (mapcar (lambda (s) (remove-duplicates (to-charlist s))) b)))) (read-input-by-3 file))))



(fiveam:test test-split-string (fiveam:is (equal '("abcd" "efgh") (split-string "abcdefgh"))))
(fiveam:test test-to-charlist (fiveam:is (equal '(#\h #\o #\l #\a) (to-charlist "hola" ))))
(fiveam:test test-find-repeated (fiveam:is (equal #\a (find-repeated "holajuan" ))))
(fiveam:test test-find-repeated (fiveam:is (equal #\a (find-repeated "holajuan" ))))
(fiveam:test test-get-score (fiveam:is (= 38 (get-score #\L))))
(fiveam:test test-process-test (fiveam:is (= 157 (read-input "day3/test"))))
(fiveam:test test-stage1 (fiveam:is (= 7980 (read-input "day3/input"))))
(fiveam:test test-find-badge (fiveam:is (equal 70 (find-badge "day3/test"))))
(fiveam:test test-stage2 (fiveam:is (equal 7980 (find-badge "day3/input"))))
(fiveam:run!)