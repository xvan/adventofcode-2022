(ql:quickload "fiveam")

;addx V - 2 cycles 
;x+v


;x t0 = 1

;noop 1 cycle

; start from 1

;multiplos de 20


(defun parse-instruction (s)
    (let ((split (split-by-one-space s)))
        (list (read-from-string (first split)) (parse-integer (second split)))))


(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :collect riv )))

(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


(defun process-addx (n state) 
    (push (getf state :acc) (getf state :hist) )
    (push (getf state :acc) (getf state :hist) )
    (incf (getf state :acc) n)
    state
)

(defun process-noop (state)
    (push (getf state :acc) (getf state :hist))
    state)

(defun process-line (s state)
    (destructuring-bind (operation &optional (argument ""))  (split-by-one-space s)
        (let ((opcode (read-from-string operation)))
         (cond ((equal 'noop  opcode) (process-noop state))
               ((equal 'addx  opcode) (process-addx (parse-integer argument) state))))))


(defun process-lines (lines)    
    (loop     
     :for line :in lines
     :for state = (process-line line (list :hist nil :acc 1)) then (process-line line state)     
     :finally (return (nreverse (getf state :hist)))))

(defun calc-strenght (hist)
    (loop
     :for item in hist
     :for n = 1 then (1+ n)
     :when (member n '(20 60 100 140 180 220)) :sum (* n item)     
    )
)

(defun process-sprite (hist)
    (loop
     :for item :in hist
     :for n = 0 then (rem (1+ n) 40)
     :when (= n 0) :do (terpri)
     :do (princ (if (and 
               (>= n (1- item))
               (<= n (1+ item))) #\# #\.))))


(process-sprite (process-lines (read-input "day10/test")))
(process-sprite (process-lines (read-input "day10/input")))

(fiveam:def-suite 10am-suite)
(fiveam:in-suite 10am-suite)

(fiveam:test test-base 
    (fiveam:is (equal '(:hist (0) :acc 0)  (process-noop '(:hist nil :acc 0))))
    (fiveam:is (equal '(:hist (0 0) :acc 1)  (process-addx 1 '(:hist nil :acc 0))))
    (fiveam:is (equal '(:hist (5 5) :acc 1)  (process-addx -4 '(:hist nil :acc 5))))
    
    (fiveam:is (equal '(:hist (0) :acc 0)    (process-line "noop" '(:hist nil :acc 0))))
    (fiveam:is (equal '(:hist (0 0) :acc 1)  (process-line "addx 1" '(:hist nil :acc 0))))
    (fiveam:is (equal '(:hist (5 5) :acc 1)  (process-line "addx -4" '(:hist nil :acc 5))))
    
    (fiveam:is (equal '(1 1 1 4 4)  (process-lines '("noop" "addx 3" "addx -5"))))
    
    )

(fiveam:test test-strength 
    (fiveam:is (equal 13140  (calc-strenght (process-lines (read-input "day10/test")))))
    (fiveam:is (equal 17840  (calc-strenght (process-lines (read-input "day10/input")))))
    )

(fiveam:test test-crt 
    (fiveam:is-true t))

(fiveam:run! '10am-suite)

'(:hist nil :acc 1)