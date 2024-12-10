(ql:quickload "fiveam")


(defvar *test_data*
"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
)


(defvar *test_data2*
"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
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


(defun parse (stack state &optional (ebl t) (stg2 nil) (acc 0) (op1 0) (op2 0))
  (if stack 
      (case (print state)
        ((nil) (cond
                  ((and ebl (eq (car stack) #\m)) (parse (cdr stack) 'm ebl stg2 acc))
                  ((eq (car stack) #\d) (parse (cdr stack) 'd ebl stg2 acc))
                  (t (parse (cdr stack) nil ebl stg2 acc))))

        (d (if (eq (car stack) #\o) (parse (cdr stack) 'do ebl stg2 acc) (parse stack nil ebl stg2 acc)))
        (do (cond
             ((eq (car stack) (code-char 40)) (parse (cdr stack) 'doP ebl stg2 acc))
             ((eq (car stack) #\n) (parse (cdr stack) 'don ebl stg2 acc))
             (t (parse stack nil ebl stg2 acc))))
        
        (doP (if (eq (car stack) (code-char 41)) (parse (cdr stack) nil t stg2 acc) (parse stack nil ebl stg2 acc)))
        
        (don (if (eq (car stack) #\') (parse (cdr stack) 'donQ ebl stg2 acc) (parse stack nil ebl stg2 acc)))
        (donQ (if (eq (car stack) #\t) (parse (cdr stack) 'donQt ebl stg2 acc) (parse stack nil ebl stg2 acc)))
        (donQt (if (eq (car stack) (code-char 40)) (parse (cdr stack) 'donQtP ebl stg2 acc) (parse stack nil ebl stg2 acc)))
        (donQtP (if (eq (car stack) (code-char 41)) (parse (cdr stack) nil (not stg2)  stg2 acc) (parse stack nil ebl stg2 acc)))

        (m (if (eq (car stack) #\u) (parse (cdr stack) 'mu ebl stg2 acc) (parse stack nil ebl stg2 acc)))
        (mu (if (eq (car stack) #\l) (parse (cdr stack) 'mul ebl stg2 acc) (parse stack nil ebl stg2 acc)))
        (mul (if (eq (car stack) (code-char 40)) (parse (cdr stack) 'fd1 ebl stg2 acc) (parse stack nil ebl stg2 acc)))
        (fd1 (if (char-digit (car stack)) (parse stack 'd1 ebl stg2 acc) (parse stack nil ebl stg2 acc)))
        (d1
         (let* ((v (char-digit (car stack)))
                (new-op (when v (+ v (* 10 op1)))))
           (cond
            ((eq (car stack) #\,) (parse (cdr stack) 'fd2 ebl stg2 acc op1))
            ((and new-op (< new-op 1000)) (parse (cdr stack) 'd1 ebl stg2 acc new-op))
            (t (parse stack nil ebl stg2 acc)))))
        (fd2 (if (char-digit (car stack)) (parse stack 'd2 ebl stg2 acc op1) (parse stack nil ebl stg2 acc)))
        (d2
         (let* ((v (char-digit (car stack)))
                (new-op (when v (+ v (* 10 op2)))))
           (cond
            ((eq (car stack) (code-char 41))
              (progn (format t "mul(~d,~d) -> acc: ~d -> ~d~%" op1 op2 acc (+ acc (* op1 op2)))
                     (parse (cdr stack) nil ebl stg2 (+ acc (* op1 op2)))))
            ((and new-op (< new-op 1000)) (parse (cdr stack) 'd2 ebl stg2 acc op1 new-op))
            (t (parse stack nil ebl stg2 acc)))))
        (otherwise (parse (cdr stack) nil ebl stg2 acc))) acc)
    )



(parse (str-to-list  "mul(123,123)") nil)
(parse (str-to-list  *test_data*) nil)
(parse (str-to-list  *test_data2*) nil t t)
(parse (read-input2 "2024/day3/test_input") nil)

(parse (read-input2 "2024/day3/input") nil)
(parse (read-input2 "2024/day3/input") nil t t)


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









