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
; (defun rotate (list-of-lists)
;   (apply #'mapcar #'list list-of-lists))

; (defun read-input (file)
;   (rotate
;   (with-open-file (in file)
;   (loop 
;                   :with iv := nil                   
;                   :for riv = (read-line in nil)                   
;                   :while riv                  
;                   :collect (loop 
;                     :with pos := 0
;                     :with ov := 0
;                     :while (if ov (multiple-value-setq (ov pos) (parse-integer riv :start pos :junk-allowed t)))
;                     :collect ov
;                   ) ))))


; (defun sort-diff-sum (pair-of-lists)
;   (apply #'+ (mapcar #'abs ( apply #'mapcar #'- (mapcar (lambda (x) (sort x #'<)) pair-of-lists)))))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defun count-repeats (test-number control-list)
;   (count-if (lambda (x) (= x test-number)) control-list))


; (defun similarity-score (test-list control-list)
; (apply #'+ (mapcar (lambda (test-number) (* test-number (count-repeats test-number control-list))) test-list)))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (sort-diff-sum (read-input "2024/day1/input"))
; (apply #'similarity-score (read-input "2024/day1/input"))

; (fiveam:test load-successfully
;   (fiveam:is (equal 
;               '((3 4 2 1 3 3) (4 3 5 3 9 3))
;               (read-input "2024/day1/test_input"))))

; (fiveam:test sort-diff-sum-succesfully
;   (fiveam:is (equal 
;               11
;               (sort-diff-sum '((3 4 2 1 3 3) (4 3 5 3 9 3))))))

; (fiveam:test count-repeats-succesfully
;   (fiveam:is (equal 
;               3
;               (count-repeats 3 '(4 3 5 3 9 3)))))              
              
; (fiveam:test calculate-similarity-score-succesfully
;   (fiveam:is (equal 
;               31
;               (similarity-score '(3 4 2 1 3 3) '(4 3 5 3 9 3)))))


; (fiveam:test calculate-similarity-score-succesfully2
;   (fiveam:is (equal 
;               31
;               (apply #'similarity-score (read-input "2024/day1/test_input")))))

; (fiveam:run!)

; (sort-diff-sum (read-input "2024/day1/input"))
; (apply #'similarity-score (read-input "2024/day1/input"))

 


; (ql:quickload "str")

; (defun solve-01-a ()
;   (let* ((input-lines (uiop:read-file-lines #p"2024/day1/input"))
;          (pairs (mapcar (lambda (line) (str:split " " line :omit-nulls t)) input-lines))
;          (left-list ())
;          (right-list ())
;          (sorted-left)
;          (sorted-right))
;     (dolist (item pairs)
;       (push (parse-integer (first item)) left-list)
;       (push (parse-integer (second item)) right-list))
;     (setf sorted-left (sort left-list #'<))
;     (setf sorted-right (sort right-list #'<))
;     (reduce #'+ (mapcar (lambda (l r) (abs (- l r))) sorted-left sorted-right))))


; (solve-01-a)

; (defun solve-01-b ()
;   (let* ((input-lines (uiop:read-file-lines #p"2024/day1/input"))
;          (pairs (mapcar (lambda (line) (str:split " " line :omit-nulls t)) input-lines))
;          (left-list ())
;          (right-list ())
;          (cache (make-hash-table :test 'equal)))
;     (dolist (item pairs)
;       (push (parse-integer (first item)) left-list)
;       (push (parse-integer (second item)) right-list))
;     (loop for left in left-list
;           unless (nth-value 1 (gethash left cache))
;             do (setf (gethash left cache) (count left right-list))
;           sum (* left (gethash left cache)))))

; (solve-01-b)


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (ql:quickload :cl-ppcre)
; (ql:quickload :split-sequence)
; (ql:quickload :lisp-utils)
; (ql:quickload :alexandria)

; (use-package :lisp-utils)

; (defun parse (lines)
;   (let ((list0 nil)
;         (list1 nil))
;     (dolist (line lines)
;       (let ((chunks (cl-ppcre:split "\\s+" line)))
;         (push (parse-integer (first chunks)) list0)
;         (push (parse-integer (second chunks)) list1)))
;     (list (sort list0 #'<)
;           (sort list1 #'<))))

; (defun compute-diffs (list0 list1)
;   (loop for i0 in list0
;         for i1 in list1
;         collecting (abs (- i0 i1))))

; (defun part1 (file-name)
;   (let ((lines (uiop:read-file-lines file-name)))
;     (destructuring-bind (list0 list1) (parse lines)
;       (apply #'+ (compute-diffs list0 list1)))))

; (defun part2 (file-name)
;   (let ((lines (uiop:read-file-lines file-name))
;         (count-occurrences (memoize (lambda (num lst)
;                                       (count num lst)))))
;     (destructuring-bind (list0 list1) (parse lines)
;       (apply #'+ (mapcar (lambda (num)
;                            (let ((occurrences (funcall count-occurrences num list1)))
;                              (* num occurrences)))
;                          list0)))))

; (print (part1 "2024/day1/input"))
; (print (part1 "input1.txt"))

; (print (part2 "input0.txt"))
; (print (part2 "input1.txt"))


; (with-open-file (stream "2024/day1/test_input")
;   (format t "~a~%" (read-line stream)))
