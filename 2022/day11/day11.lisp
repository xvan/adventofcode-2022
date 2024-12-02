(ql:quickload "fiveam")

(defun split-by-one-char (string char)
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))

(defun parse-id (input)
    (parse-integer (first (split-by-one-char (second (split-by-one-char (string-trim '(#\Space #\Tab) input) #\Space )) #\:))))

(defun parse-items (input)
    (mapcar 'parse-integer (split-by-one-char  (remove #\, (string-trim "Starting items:" (string-trim '(#\Space #\Tab) input))) #\Space)))
    
(defun parse-operation (input)
    (destructuring-bind (arg1 op arg2)
        (mapcar 'read-from-string  (cdr (split-by-one-char (string-trim "Operation: new" (string-trim '(#\Space #\Tab) input)) #\Space)))
        (coerce `(lambda (old) (,op ,arg1 ,arg2)) 'function )
    ))

(defun split-last-is-integer (s)
    (parse-integer (car (nreverse (split-by-one-char s #\Space))))
    )

(defun load-monkey (in)
    (let* 
            (
             (id-input (parse-id (read-line in nil)))
             (items-input (parse-items (read-line in nil)))
             (operation-input (parse-operation (read-line in nil)))
             (divisor (split-last-is-integer (read-line in nil)))
             (true-rval (split-last-is-integer (read-line in nil)))
             (false-rval (split-last-is-integer (read-line in nil)))
             (dummy (read-line in nil))
             )
    dummy; dummy usage
    (list 
     :id id-input
     :items items-input
     :operation operation-input
     :test (lambda (x) (if (= 0 (rem x divisor)) true-rval false-rval))
     :divisor divisor
    )))

(defun read-input (file)
  (apply 'vector (with-open-file (in file)    
    (loop  :while (listen in)      
      collect (load-monkey in))        
        )))

(defun report-monkeys (monkeys)    
    (loop :for n from 0 :below (length monkeys)
          :as monkey = (aref monkeys n)
          :do (getf monkey :items))
    monkeys
    )
        
(defun step-monkeys (monkeys lastsum worry-reduction)
    ;(report-monkeys monkeys)
    (loop :for n from 0 :below (length monkeys)
              :collect (loop :while (getf (aref monkeys n) :items)
                        :do (let ((item (funcall worry-reduction (funcall (getf (aref monkeys n) :operation) (pop (getf (aref monkeys n) :items))))))
                            (push item (getf (aref monkeys (funcall (getf (aref monkeys n) :test) item)) :items)))
                        :sum 1
               ;:finally (report-monkeys monkeys)
                        ) :into popsum
              :finally (return (list monkeys (mapcar '+ lastsum popsum)))
              ))


(defun gen-zeros (start-monkeys)
    (loop for i from 0 below (length start-monkeys) collect 0)
    )

(defun calc-mod (start-monkeys)
    (apply '* (loop for i from 0 below (length start-monkeys) collect (getf (aref start-monkeys i) :divisor))))


(defun count-items (start-monkeys worry-reduction rounds)
(do (     
     (monkeys-sum (list start-monkeys (gen-zeros start-monkeys)) (apply (lambda (x y) (step-monkeys x y worry-reduction)) monkeys-sum))
     (n 0 (1+ n))     
     )
    ((= n rounds) (second monkeys-sum))
    ))


(defun process-first (start-monkeys)
    (let ((itemcount (sort (count-items start-monkeys (lambda (x) (floor x 3)) 20) '>)))
        (* (first itemcount) (second itemcount))))

(defun process-second (start-monkeys rounds)
    
    (let* (
           (numod (print (calc-mod start-monkeys)))
           (itemcount (sort (count-items start-monkeys (lambda (x) (mod x numod)) rounds) '>))
           )
        (print itemcount)
        (* (first itemcount) (second itemcount))))

;(process-first (read-input "day11/input"))

(fiveam:def-suite 11am-suite)
(fiveam:in-suite 11am-suite)

(fiveam:test test-parser 
    (fiveam:is (equal 0 (parse-id "Monkey 0:")))
    (fiveam:is (equal '(79 98) (parse-items "Starting items: 79, 98")))
    (fiveam:is (equal '(54 65 75 74) (parse-items "  Starting items: 54, 65, 75, 74")))
    (fiveam:is (equal 20 (funcall (parse-operation "  Operation: new = old * 10") 2)))
    (fiveam:is (equal 6 (funcall (parse-operation "  Operation: new = old + old") 3)))

    (fiveam:is (equal 1 (funcall (parse-test 
                                     "  Test: divisible by 13"
                                     "    If true: throw to monkey 1"
                                     "    If false: throw to monkey 3") 26)))
    
    (fiveam:is (equal 3 (funcall (parse-test 
                                     "  Test: divisible by 13"
                                     "    If true: throw to monkey 1"
                                     "    If false: throw to monkey 3") 27)))
    
    (fiveam:is (read-input "day11/test"))
    )

(fiveam:test test-process 
    (fiveam:is (equal 10605 (process-first (read-input "day11/test"))))
    (fiveam:is (equal 117640 (process-first (read-input "day11/input"))))
    (fiveam:is (equal 2713310158  ( process-second  (read-input "day11/test") 10000 ) ))
    (fiveam:is (equal 30616425600 ( process-second  (read-input "day11/input") 10000 ) ))
    )

(fiveam:run! '11am-suite)