(ql:quickload "fiveam")


(defun hashpairs (keys values)
    (let ((table (make-hash-table)))
        (loop for key in keys for value in values
                  do (setf (gethash key table) value))
        table))


(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :collect (mapcar 'read-from-string (split-by-one-space riv)))))


(defun test-hashpair () (hashpairs '(X Y Z) '(A B C)) )


(defun apply-key (hashpair data) 
    (loop for pair in data
              :collect (list (first pair) (gethash (second pair) hashpair))))


(defun rules (theirs ours)
    (let ((hand (list theirs ours)))
        (cond ((equal hand '(A A) ) (+ 3 1) )
              ((equal hand '(A B) ) (+ 6 2) )
              ((equal hand '(A C) ) (+ 0 3) )
              ((equal hand '(B A) ) (+ 0 1) )
              ((equal hand '(B B) ) (+ 3 2) )
              ((equal hand '(B C) ) (+ 6 3) )
              ((equal hand '(C A) ) (+ 6 1) )
              ((equal hand '(C B) ) (+ 0 2) )
              ((equal hand '(C C) ) (+ 3 3) )
              )))
        


(defun decode (theirs ours)
    (let ((hand (list theirs ours)))
        (cond ((equal hand '(A Z) ) 'B )
              ((equal hand '(A X) ) 'C )
              ((equal hand '(A Y) ) 'A )
              ((equal hand '(B Z) ) 'C )
              ((equal hand '(B X) ) 'A )
              ((equal hand '(B Y) ) 'B )
              ((equal hand '(C Z) ) 'A )
              ((equal hand '(C X) ) 'B )
              ((equal hand '(C Y) ) 'C )
              )))

(defun apply-decode (data)
    (loop for pair in data
            :collect (list (first pair) (apply 'decode pair))))    
    


(defun apply-rules (hands)
    (apply '+ (mapcar (lambda (h) (apply 'rules h)) hands)))


(apply-rules (apply-key (hashpairs '(X Y Z) '(A B C)) (read-input "day2/input")))



(fiveam:test hashpairs (fiveam:is (eql 'C (gethash 'Z (hashpairs '(X Y Z) '(A B C))))))
(fiveam:test rfs (fiveam:is (eql 'A (read-from-string "A"))))
(fiveam:test test-data (fiveam:is (equal '((A Y) (B X) (C Z)) (read-input "day2/test"))))
(fiveam:test test-apply-hashpair (fiveam:is (equal '((A B) (B A) (C C)) (apply-key (test-hashpair) '((A Y) (B X) (C Z)) ))))
(fiveam:test test-rules (fiveam:is (= 7 (rules 'C 'A))))
(fiveam:test test-apply-rules (fiveam:is (= 15 (apply-rules '((A B) (B A) (C C)) ))))


(fiveam:test test-naive-solve (fiveam:is (= 12772 (apply-rules (apply-key (hashpairs '(X Y Z) '(A B C)) (read-input "day2/input"))))))
(fiveam:test test-decode-solve (fiveam:is (= 11618 (apply-rules (apply-decode (read-input "day2/input"))))))


;(fiveam:test test-data (fiveam:is ((read-input "day2/test"))))


(fiveam:run!)