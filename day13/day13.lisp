(ql:quickload "fiveam")

(defun read-input (file)
  (with-open-file (in file)
    (loop  
         :while (listen in) 
         :collect ( list (parse-line (read-line in nil)) (parse-line (read-line in nil)) )
         :do (read-line in nil) ; discard separation
         )))


(defun parse-line (string)
    (let ((stack (list nil)))
     
     (loop 
          for i = 0 then (1+ j)
          as j = (position-if (lambda (c) (member c '(#\[ #\] #\,))) string :start i)
          as n = (parse-integer string :start i :end j :junk-allowed t)         
          when n do (push n (car stack))
          while j
          do (cond
            ((eql #\[ (aref string j)) (push nil stack))
            ((eql #\] (aref string j)) (push (nreverse (pop stack)) (car stack))))
          :finally (return (car (pop stack))))
        ))


(defun list-head-comparer (left right)
    (cond
     ((and (null left) (null right)) :equal)
     ((null left) :good)
     ((null right) :bad)
     (t (packet-comparer (car left) (car right)))
    )
)

(defun packet-comparer (left right)
    (cond
     ((equal left right) :equal)
     ((equal nil left) :good)
     ((equal nil right) :bad)
     ((and (numberp left) (numberp right)) (if (< left right) :good :bad))
     ((and (numberp left) (numberp right)) (if (< left right) :good :bad))
     ((numberp left) (packet-comparer (cons left nil) right))
     ((numberp right) (packet-comparer left (cons right nil) ))
     ;default: ambos listas
     (t 
         (do (
              (l left (cdr l))
              (r right (cdr r))
              (test :equal (list-head-comparer l r))
              ) 
                 ((not (eql :equal test)) test)
             ))))

(defun compare-many (pairs-input)
    (mapcar (lambda (pair) (apply 'packet-comparer pair)) pairs-input))


(defun count-good-index (compare-results)
    (loop
     :for result :in compare-results
     :for n = 1 :then (1+ n)
     :when (eql :good result) :sum n
     ))


(defun flatten-and-append-divider (pairs-input divider)
    (append divider (apply 'nconc pairs-input)))

;(defun decoder-key (pair-list)
;    (let* (
;           (divider '(((2)) ((6))))
;           (sorted (sort (flatten-and-append-divider pair-list divider) (lambda (a b) (not(eql :bad (packet-comparer a b))))))
;           )
;           (apply '* (mapcar (lambda (x) (1+ (position x sorted :test 'equal))) divider)))        
;    )


(defun decoder-key (pair-lists)
    (let* (
           (divider '(((2)) ((6))))
           (flat (flatten-and-append-divider pair-lists divider)))
        (apply '* (mapcar (lambda (c) (
                             loop for p in flat                                  
                                      when (not (eql :bad (packet-comparer p c))) sum 1)) divider))
        ))



(fiveam:def-suite 13am-suite)
(fiveam:in-suite 13am-suite)

(fiveam:test test-parser
    (fiveam:is (equal nil (parse-line "")))
    (fiveam:is (equal nil (parse-line "[]")))
    (fiveam:is (equal '(1) (parse-line "[1]")))
    (fiveam:is (equal '(1 2) (parse-line "[1,2]")))
    (fiveam:is (equal '(1 1 3 1 1) (parse-line "[1,1,3,1,1]")))
    (fiveam:is (equal '((1)(2 3 4)) (parse-line "[[1],[2,3,4]]")))
    (fiveam:is (equal '((4 4) 4 4) (parse-line "[[4,4],4,4]")))
    (fiveam:is (equal '((4 4) 4 4) (parse-line "[[4,4],4,4]")))
    (fiveam:is (equal '((())) (parse-line "[[[]]]")))
    (fiveam:is (equal '(1 (2 (3 (4 (5 6 7)))) 8 9) (parse-line "[1,[2,[3,[4,[5,6,7]]]],8,9]")))    

    )

(fiveam:test test-input
    (fiveam:is-true (read-input "day13/test"))
    (fiveam:is (equal '(((2)) ((6)) a b c d e f) (flatten-and-append-divider '((a b) (c d) (e f)) '(((2)) ((6))) )))    
    )


(fiveam:test test-comparer
    (fiveam:is (eql :equal (packet-comparer nil nil)))
    (fiveam:is (eql :good (packet-comparer nil 1)))
    (fiveam:is (eql :bad (packet-comparer 1 nil)))
    (fiveam:is (eql :good (packet-comparer nil '(1))))
    (fiveam:is (eql :bad (packet-comparer '(1) nil)))
    (fiveam:is (eql :equal (packet-comparer 1 1)))
    (fiveam:is (eql :good (packet-comparer 1 2)))
    (fiveam:is (eql :bad (packet-comparer 2 1)))
    (fiveam:is (eql :equal (packet-comparer '(1) '(1))))
    (fiveam:is (eql :good (packet-comparer '(1) '(2))))
    (fiveam:is (eql :bad (packet-comparer '(2) '(1))))
    (fiveam:is (eql :equal (packet-comparer '(1 1) '(1 1))))
    (fiveam:is (eql :good (packet-comparer '(1 1) '(1 2))))
    (fiveam:is (eql :bad (packet-comparer '(1 2) '(1 1))))
    (fiveam:is (eql :equal (packet-comparer '(1 1 3) '(1 1 3))))
    (fiveam:is (eql :good (packet-comparer '(1 1 3) '(1 2 3))))
    (fiveam:is (eql :bad (packet-comparer '(1 2 3) '(1 1 3))))
    (fiveam:is (eql :good (packet-comparer '(1 2 3) '(1 2 3 4))))   
    (fiveam:is (eql :bad (packet-comparer '(1 2 3 4) '(1 2 3))))    
    (fiveam:is (eql :good (packet-comparer '((6)) '((6 nil)))))
    (fiveam:is (eql :bad (packet-comparer '((6 nil)) '((6)))))
    )

(fiveam:test test-process
    (fiveam:is  (equal '(:GOOD :GOOD :BAD :GOOD :BAD :GOOD :BAD :BAD) (compare-many (read-input "day13/test"))))
    (fiveam:is  (= 13 (count-good-index '(:GOOD :GOOD :BAD :GOOD :BAD :GOOD :BAD :BAD))))
    (fiveam:is  (= 5196 (count-good-index (compare-many (read-input "day13/input")))))
    (fiveam:is  (= 140 (decoder-key (read-input "day13/test"))))
    (fiveam:is  (= 22134 (decoder-key (read-input "day13/input"))))
    )

;(read-input "day13/input")

(fiveam:run! '13am-suite)


(let ((x  '(nil nil)))
    (push (pop x) (car x))
    )