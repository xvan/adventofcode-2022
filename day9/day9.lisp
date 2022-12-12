(ql:quickload "fiveam")


(defun move-head (dir history)
    (cond
     ((equal dir 'U) (change-history-reference '(-1  0) history))
     ((equal dir 'D) (change-history-reference '( 1  0) history))
     ((equal dir 'L) (change-history-reference '( 0  1) history))
     ((equal dir 'R) (change-history-reference '( 0 -1) history))
     ))


(defun change-history-reference (ref-change history) ()
    (mapcar (lambda (p) (mapcar '+ p ref-change)) history))

(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun parse-instruction (s)
    (let ((split (split-by-one-space s)))
        (list (read-from-string (first split)) (parse-integer (second split)))))


(defun resolve-move (dir)
    (cond
     ((equal dir 'U) '(-1  0))
     ((equal dir 'D) '( 1  0))
     ((equal dir 'L) '( 0  1))
     ((equal dir 'R) '( 0 -1))
     ))

(defun explode-instructions (instructions)
    (loop :for inst :in (mapcar 'parse-instruction instructions)
        :append (loop :for n :from 1 :to (second inst)
                    :collect (resolve-move (first inst)))
    ))


(defun execute-step (ref-change history update-strategy)    
        (setf history (change-history-reference ref-change history))
        (setf history (funcall update-strategy history)))

(defun execute-steps (exploded-instructions history update-strategy)
    (dolist (ref-change exploded-instructions history)
        (setf history (execute-step ref-change history update-strategy))))        


(defun process-instructions (instructions history update-strategy) 
    (execute-steps (explode-instructions instructions) history update-strategy))
    

(defun history-is-head-strategy (history) (cons '(0 0) history))

(defun history-is-tail-strategy (history) 
    (let ((tail (car history)))
        (cond
        
         ((equal '( 2  2) tail) (cons '( 1  1) history))
         ((equal '( 2 -2) tail) (cons '( 1 -1) history))
         ((equal '(-2  2) tail) (cons '(-1  1) history))
         ((equal '(-2 -2) tail) (cons '(-1 -1) history))

         ((= 2 (first tail)) (cons '(1 0) history))
         ((= -2 (first tail)) (cons '(-1 0) history))
         ((= 2 (second tail)) (cons '(0 1) history))
         ((= -2 (second tail)) (cons '(0 -1) history))

         (t history)
         )
     ))


(defun invert-y (history) (let ((max-y (apply 'max (mapcar 'first history))))
                           (mapcar (lambda (b) (list (* -1 (- (first b) max-y)) (second b)) ) history))
 )


(defun calc-map-reference (history)
    (mapcar (lambda (b) (* -1 b)) (list (apply 'min (mapcar 'first history)) (apply 'min (mapcar 'second history)))))

(defun calc-bounds (history)
    (mapcar '1+ (list (apply 'max (mapcar 'first history)) (apply 'max (mapcar 'second history)))))


(defmacro pair-ref (arr idx)
    `(apply #'aref (cons ,arr ,idx)))


(defun history-to-map (history)
    (let* ((rhistory (reverse (history-is-head-strategy history)))
           (offset-history (invert-y  (change-history-reference  (calc-map-reference rhistory) rhistory)))
           
           (map (make-array (calc-bounds offset-history))))
                
        
        (do ((c offset-history (cdr c)))
                ((not c) map)            
            (setf (pair-ref map (car c)) 2))        
        
        (setf (pair-ref map (car offset-history)) 1)
        (let ((heads (nreverse offset-history)))
            (setf (pair-ref map (car (cdr heads))) 3)
            (setf (pair-ref map (car heads)) 4))
        map
    ))


(defun print-map (array)
  (terpri)
  (loop for i from 0 below (array-dimension array 0)
        do (loop for j from 0 below (array-dimension array 1)
                 do (princ (aref array i j))
                 (if (= j (1- (array-dimension array 1)))
                     (terpri)))))


(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :collect riv )))

(print-map (history-to-map (process-instructions '("U 1" "U 1" "R 1") '((0 0)) 'history-is-head-strategy)))
(print-map (history-to-map (process-instructions (read-input "day9/test") '((0 0)) 'history-is-tail-strategy)))

(print-map (history-to-map (process-instructions (read-input "day9/test2") '((0 0)) 'history-is-tail-strategy)))



(defun steps-from-history (history)
        (let* ((rhistory (reverse history))
               (offset-history (change-history-reference  (car rhistory) rhistory)))
        
    (mapcar (lambda (b c) (mapcar '- c b)) (cdr offset-history) offset-history))
)

(steps-from-history (print (process-instructions (read-input "day9/test") '((0 0)) 'history-is-tail-strategy)))

(defun process-input-first (filepath)
    (length 
        (remove-duplicates                 
                (process-instructions (read-input filepath) '((0 0)) 'history-is-tail-strategy)
            :test 'equal )))


(defun process-input-n-knots (filepath n)
    (length 
        (remove-duplicates                 
                (process-n-knots  (read-input filepath) n 'history-is-tail-strategy)
            :test 'equal )))



(defun process-n-knots (instructions n strategy)
    (loop
     :for i :from 1 :to n
     :for moves = (explode-instructions instructions) then (steps-from-history history)
     :as history = '((0 0))
     :do (setf history (execute-steps moves history strategy))
     :finally (return history))
    )




(process-n-knots (read-input "day9/test")  1 'history-is-tail-strategy)

(process-n-knots (read-input "day9/test")  2 'history-is-head-strategy)

(fiveam:def-suite 9am-suite)
(fiveam:in-suite 9am-suite)

(fiveam:test history-test
    (let ((test-history '((0 0) (0 1) (0 -1) (1 0) (1 -1))))        
        (fiveam:is (equal '((-1 0) (-1 1) (-1 -1) (0 0) (0 -1)) (change-history-reference '(-1  0) test-history)))
        (fiveam:is (equal '((0 1) (0 2) (0 0) (1 1) (1 0)) (change-history-reference '( 0  1) test-history)))
        (fiveam:is (equal '((1 0) (1 1) (1 -1) (2 0) (2 -1)) (change-history-reference '( 1  0) test-history)))
        (fiveam:is (equal '((0 -1) (0 0) (0 -2) (1 -1) (1 -2)) (change-history-reference '( 0 -1) test-history)))

        (fiveam:is (equal '((-1 0)) (move-head 'U '((0 0)))))
        (fiveam:is (equal '((1 0)) (move-head 'D '((0 0)))))
        (fiveam:is (equal '((0 1)) (move-head 'L '((0 0)))))
        (fiveam:is (equal '((0 -1)) (move-head 'R '((0 0)))))
        ))
    
(fiveam:test instruction-parser
    (fiveam:is (equal '(S 4) (parse-instruction "S 4"))))

(fiveam:test test-rope    
    (fiveam:is (equal 13 (process-input-first "day9/test")) )
    (fiveam:is (equal 6181 (process-input-first "day9/input")) )

    (fiveam:is (equal 13 (process-input-n-knots "day9/test" 1)) )
    (fiveam:is (equal 6181 (process-input-n-knots "day9/input" 1)) )

    (fiveam:is (equal 13 (process-input-n-knots "day9/test" 1)) )
    (fiveam:is (equal 6181 (process-input-n-knots "day9/input" 1)) )


    (fiveam:is (equal (process-n-knots (read-input "day9/test")  10 'history-is-head-strategy) (process-n-knots (read-input "day9/test")  1 'history-is-head-strategy)))

    (fiveam:is (equal 36 (process-input-n-knots "day9/test2" 9)) )
    (fiveam:is (equal 13 (process-input-n-knots "day9/input" 9)) )
)

(fiveam:run! '9am-suite)