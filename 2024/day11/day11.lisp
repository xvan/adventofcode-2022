(ql:quickload "cl-ppcre")

(defun read-input (file)
  (with-open-file (in file)    
    (cl-ppcre:split "(:?\\s+)" (read-line in nil) )))

(defun str->int->str (s)
  (int->str (parse-integer s :junk-allowed t)))

(defun int->str (n)
  (format nil "~d" n))

(defun blink (rocks)
  (loop 
    :for rock :in rocks
    :append (blink1 rock)))


(defun blink1 (rock)
  (let* ((len (length  rock))
         (mid (floor len 2)))
    (cond
           ((equalp rock "0") '("1"))
           ((evenp len) (list (str->int->str (subseq rock 0 mid)) (str->int->str  (subseq rock mid))))
           (t (list (int->str (* 2024 (parse-integer rock :junk-allowed t)))))
           ))   
  )


(defun solve-first (rocks)
  (loop
    :for rocks2 := rocks :then (blink rocks2)
    :for i :from 0 :below 25
    :finally (return rocks2)   
   )  
  )

(defun recursive-solve (rocks n)
  (if (= n 0) 
      (length rocks) 
      (loop :for rock :in rocks            
      :sum (recursive-solve (blink1 rock) (1- n)))
  ))
  


(defun init-catbag (rocks)
  
    (loop 
     :with catbag := (make-hash-table :test 'equalp)
     :for rock :in rocks
     :do (incf (gethash rock catbag 0)))     
    )

(defun update-catbag (catbag)
  (let ((new-catbag (make-hash-table :test 'equalp)))
    (maphash (lambda (k v) 
             (loop :for rock :in (blink1 k)
              :do (incf (gethash rock new-catbag 0) v)))
             catbag)
    new-catbag
    ))

(defun count-rocks (catbag)
  (let ((cnt 0))
    (maphash (lambda (k v) (incf cnt v)) catbag)
    cnt
    )
  )

(defun solve-second (rocks n)
(loop
    :for catbag := (init-catbag rocks) :then (update-catbag catbag)
    :for i :from 0 :below n
    :finally (count-rocks catbag)
   )
)

(defun deduplicate (l)
  (let ((hash (make-hash-table :test 'equal))
        (duplicates nil))
    (dolist (item l)
      (incf (gethash item hash 0)))
    (maphash (lambda (key value)
               (push (list key value) duplicates)) hash)            
    duplicates))

(deduplicate '(1 1 2 2 3 4 4)) 

(defun solve (file n)
  (let ((rocks (read-input file)))
    (cond 
     ((= n 1) (length (solve-first rocks)))
     ((= n 2) (solve-second rocks 25))
    )))

;(untrace next-empty-range)
;(trace find-empty-range)
;(untrace next-empty-range)
;(untrace next-file)


;(solve-first (read-input "2024/day11/test_input1" ))

;(solve "2024/day11/test_input0" 1)
(solve "2024/day11/test_input1" 1)
(solve "2024/day11/input" 1)

(solve "2024/day11/test_input1" 2)
(solve "2024/day11/input" 2)
