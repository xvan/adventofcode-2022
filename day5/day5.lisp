(ql:quickload "fiveam")

(defun parse-header-lines (lines)
    (parse-stacks (make-stacks (car lines)) (cdr lines)))

(defun parse-stacks (stacks lines)
    (loop for line in lines
          do (loop for stack in stacks
                       as coord = (getf stack :charcol)
                       as symbol-substring = (subseq line coord (1+ coord))
                       when (string-not-equal " " symbol-substring) 
                            do (push (read-from-string symbol-substring) (getf stack :stack)))
          finally (return (mapcar (lambda (s) (getf s :stack)) stacks))))

(defun read-header (in)
    (reverse (loop  
        :for riv = (read-line in nil)
        :while (not (equal "" riv))
        :collect riv)))

(defun read-instructions (in)
    (loop  
        :for riv = (read-line in nil)
        :while riv
        :collect riv ))


(defun read-input (file instruction-parser)
  (with-open-file (in file)
        (let* ((stacks (parse-header-lines (read-header in))) (instructions (mapcar 'parse-instruction (read-instructions in))))
            (loop for instruction in instructions                
                do (funcall instruction-parser instruction stacks)
                finally (return stacks))            
            )))


(defun solve-first (file)
    (apply #'concatenate 'string (mapcar (lambda (b) (symbol-name (car b))) (read-input file 'execute-instruction))))


(defun solve-second (file)
    (apply #'concatenate 'string (mapcar (lambda (b) (symbol-name (car b))) (read-input file 'execute-instruction-2))))

(defun execute-instruction (instruction stacks)    
        (dotimes (n (first instruction))
            (push (pop (elt stacks (1- (second instruction)))) (elt stacks (1- (third instruction)))))
        stacks )


(defun execute-instruction-2 (instruction stacks)    
        (loop for item in (reverse (loop for n from 1 to (first instruction) 
              collect (pop (elt stacks (1- (second instruction))))))
         do (push item (elt stacks (1- (third instruction))))                       
        )
        stacks)

(defun to-charlist (s) (coerce s 'list))


(defun make-stacks (line)
    (loop for c in (to-charlist line)
          for i = 0 then (1+ i)
          when (char-not-equal #\Space c) collect (list :coord (parse-integer (string c)) :charcol i :stack '() )))

(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun parse-instruction (instruction)
    (loop for element in (split-by-one-space instruction)
          as parsed = (parse-integer element :junk-allowed t)
          when parsed collect parsed ))

(fiveam:test test-make-stacks (fiveam:is (equal 
                                          '((:COORD 1 :CHARCOL 1 :STACK NIL) (:COORD 2 :CHARCOL 5 :STACK NIL)(:COORD 3 :CHARCOL 9 :STACK NIL))
                                          (make-stacks " 1   2   3 "))))


(fiveam:test test-parse-header (fiveam:is (equal 
                                           '((N Z) (D C M) (P)) 
                                           (parse-header-lines
                                               (reverse '("    [D]    "
                                                          "[N] [C]    "
                                                          "[Z] [M] [P]"
                                                          " 1   2   3 "))))))

(fiveam:test test-parse-instruction (fiveam:is (equal '(21 222 5) (parse-instruction "move 21 from 222 to 5"))))
(fiveam:test test-read (fiveam:is  (equal '((C) (M) (Z N D P)) (read-input "day5/test" 'execute-instruction ))))
(fiveam:test test-solve-test (fiveam:is  (equal "CMZ" (solve-first "day5/test"))))
(fiveam:test test-solve-input (fiveam:is  (equal "VQZNJMWTR" (solve-first "day5/input"))))
(fiveam:test test-solve-test-2 (fiveam:is  (equal "MCD" (solve-second "day5/test"))))
(fiveam:test test-solve-input-2 (fiveam:is  (equal "NLCDCLVMQ" (solve-second "day5/input"))))
(fiveam:run!)
