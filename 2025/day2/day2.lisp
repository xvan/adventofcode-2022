(ql:quickload "fiveam")

(ql:quickload "cl-ppcre")

; (defun str-to-list (str)
; (loop :for c :across str :collect c))

; (defun list-to-2d-array (list)
;   (make-array (list (length list)
;                     (length (first list)))
;               :initial-contents list))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :append (mapcar #'(lambda (x) (mapcar #' parse-integer(cl-ppcre:split "-" x))) (cl-ppcre:split "," riv ));(mapcar #'parse-integer (cl-ppcre:split "," riv ))
  )))


(defun str-to-list (str)
(loop :for c :across str :collect c))


(read-input "2025/day2/test_input")

(defun explode-int (n )
(reverse (loop 
 :for (r c) := (multiple-value-list (floor n 10)) :then (multiple-value-list (floor r 10))
      :collect c
      :until (eq r 0 )       
)))


(defun one-range (s f)
      (loop
            :for i :from s :upto f
            :for l := (explode-int i)
            :for mid := (floor (length l) 2)
            :when (equalp (subseq l 0 mid) (subseq l mid))
            :sum i
      ))

(defun problem1 (path)
(loop 
 :for (s f) :in (read-input path)
 :sum (one-range s f))
)

(problem1 "2025/day2/test_input")
(problem1 "2025/day2/input")

(defun taken (lst n)      
      (loop
       :with ph := lst       
       :for tk := (subseq ph 0 n)
       :do (setf ph (subseq ph n))
       :collect tk
       :while ph
       ))


(defun extended-equalp (lst) 
      (every #'identity (mapcar #'equalp lst (cdr lst))))

(defun test-all-splits (l)
(loop
 :for mid :from 1 :upto (floor (length l) 2)
 :when (and 
            (eq (mod (length l) mid) 0)
            (extended-equalp (taken l mid)))
 :return t
 )
)

(defun one-range2 (s f)
      (loop
            :for i :from s :upto f
            :for l := (explode-int i)            
            :when (test-all-splits l)
            :sum i            
      ))

(problem2 "2025/day2/test_input")
(problem2 "2025/day2/input")


