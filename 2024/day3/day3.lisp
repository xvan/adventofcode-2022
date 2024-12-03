(ql:quickload "fiveam")


(defvar *test_data*
"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
)


(defun read-input (file)
  (with-open-file (in file)
  (read-line in nil)))

(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun char-digit (c) 
  (let ((v (- (char-code c) (char-code #\0))))
  (when (<= 0 v 9) v)
  ))


(code-char 41)

(defun parse (stack state &optional (acc 0) )
  (+ acc (if stack 
             (case (print state)
              ((nil) (if (eq (car stack) #\m) (parse (cdr stack) 'm) (parse (cdr stack) nil)))
              (m (if (eq (car stack) #\u) (parse (cdr stack) 'mu ) (parse stack nil)))
              (mu (if (eq (car stack) #\l) (parse (cdr stack) 'mul ) (parse stack nil)))
              (mul (if (eq (car stack) (code-char 40)) (parse (cdr stack) 'fd1) (parse stack nil)))
              (fd1 (if (char-digit (car stack)) (parse stack 'd1) (parse stack nil)))
              (d1 
                (let* ( (v (char-digit (car stack)))
                        (new-op (when v (+ v (* 10 acc)))))                
                      (cond
                          ((eq (car stack) #\,) (parse (cdr stack) 'fd2))                   
                          ( (and new-op (< new-op 1000)) (parse (cdr stack) 'd1 new-op))
                          (t (parse stack nil))
                          )))
              (fd2 (if (char-digit (car stack)) (parse stack 'd2) (parse stack nil)))
              (d2
                (let* ( (v (char-digit (car stack)))
                      (new-op (when v (+ v (* 10 acc)))))                
                    (cond
                        ((eq (car stack) (code-char 40)) (parse (cdr stack) 'fd2))                   
                        ( (and new-op (< new-op 1000)) (parse (cdr stack) 'd2 new-op))
                        (t (parse stack nil))
                        )))
              (otherwise (parse (cdr stack) nil))
               ) 0
              )
     ))

(parse (str-to-list  "mul(1,2)") nil)


(defun boolean-to-integer (value)
  (if value 1 0))

(defun safe-reports (data)
  (apply #'+ (mapcar #'boolean-to-integer (mapcar #'is-safe data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun variations (test_line)
  (loop for idx below (length test_line) collect
          (loop :for i :in test_line 
              :for jdx :from 0
              :unless (= jdx idx) :collect i)
          )  
  )

(defun is-safe-with-tolerance (test_line)
    (some #'identity (mapcar #'is-safe (variations test_line))))

(defun safe-reports-with-tolerance (data)
  (apply #'+ (mapcar #'boolean-to-integer (mapcar #'is-safe-with-tolerance data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (safe-reports (read-input "2024/day3/input")))
(print (safe-reports-with-tolerance (read-input "2024/day3/input")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test load-successfully
  (fiveam:is (equal 
              *test_data*
              (read-input "2024/day3/test_input"))))


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









