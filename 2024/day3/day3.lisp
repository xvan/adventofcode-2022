(ql:quickload "fiveam")


(defvar *test_data*
"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
)


(defun read-input (file)
  (with-open-file (in file)
  (read-line in nil)))

(defun read-input2 (file)
  (with-open-file (in file)
    (loop :for riv = (read-char in nil) :while riv :collect riv)
    )
  )

(read-input2 "")

(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun char-digit (c) 
  (let ((v (- (char-code c) (char-code #\0))))
  (when (<= 0 v 9) v)
  ))


(defun parse (stack state &optional (acc 0) (op1 0) (op2 0))
  (if stack 
             (case state
              ((nil) (cond
                  ((eq (car stack) #\m) (parse (cdr stack) 'm acc))
                  (t (parse (cdr stack) nil acc)))
                )
        
              (m (if (eq (car stack) #\u) (parse (cdr stack) 'mu acc ) (parse stack nil acc)))
              (mu (if (eq (car stack) #\l) (parse (cdr stack) 'mul acc ) (parse stack nil acc)))
              (mul (if (eq (car stack) (code-char 40)) (parse (cdr stack) 'fd1 acc) (parse stack nil acc)))
              (fd1 (if (char-digit (car stack)) (parse stack 'd1 acc) (parse stack nil acc)))
              (d1 
                (let* ( (v (char-digit (car stack)))
                        (new-op (when v (+ v (* 10 op1)))))
                      (cond
                          ((eq (car stack) #\,) (parse (cdr stack) 'fd2 acc op1))                   
                          ( (and new-op (< new-op 1000)) (parse (cdr stack) 'd1 acc new-op))
                          (t (parse stack nil acc))
                          )))
              (fd2 (if (char-digit (car stack)) (parse stack 'd2 acc op1) (parse stack nil acc)))
              (d2
              (let* ( (v (char-digit  (car stack)))
                      (new-op (when v (+ v (* 10 op2)))))                
                    (cond
                        ((eq (car stack) (code-char 41))
                          (progn (format t "mul(~d,~d) -> acc: ~d -> ~d~%" op1 op2 acc (+ acc (* op1 op2)))
                          (parse (cdr stack) nil (+ acc (* op1 op2)))))
                        ( (and new-op (< new-op 1000)) (parse (cdr stack) 'd2 acc op1 new-op))
                        (t (parse stack nil acc))
                        )))
              (otherwise (parse (cdr stack) nil acc))
               ) acc
              )
    )




(parse (str-to-list  "mul(123,123)") nil)
(parse (str-to-list  *test_data*) nil)
(parse (read-input2 "2024/day3/test_input") nil)

(parse (read-input2 "2024/day3/input") nil)


(defun boolean-to-integer (value)
  (if value 1 0))

(defun safe-reports (data)
  (apply #'+ (mapcar #'boolean-to-integer (mapcar #'is-safe data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(print (safe-reports (read-input "2024/day3/input")))
;;(print (safe-reports-with-tolerance (read-input "2024/day3/input")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (fiveam:test load-successfully
;   (fiveam:is (equal 
;               *test_data*
;               (read-input "2024/day3/test_input"))))


(fiveam:test parser-end-condition
  (fiveam:is (equal 
              nil              
              (parse () 'm))))

(fiveam:test parser-one-mul
  (fiveam:is (equal 
              6
              (parse (str-to-list  "mul(2,3)") 'm))))

(fiveam:test parser-advance-no-match
  (fiveam:is (equal 
              nil              
              (parse (str-to-list  "abc") 'm))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fiveam:run!)









