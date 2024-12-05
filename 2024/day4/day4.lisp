(ql:quickload "fiveam")

(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (str-to-list riv) )))




(defvar *test_data* (read-input "2024/day4/test_input"))






(defun rot-left(n l)
  (append (nthcdr n l) (butlast l (- (length l) n))))

(defun rot-right(n l)
  (rot-left (- (length l) n) l))
 

(defun rot-45 (input)
(rotate (loop :with inputT = (rotate input)
      :with padding = ( make-list (- (length inputT) 1) )
      :for line in inputT
      :for n :from 0
      :collect  (rot-right n (append  line padding)
        ))))

(defun rot+45 (input)
(rotate (loop :with inputT = (rotate input)
      :with padding = ( make-list (- (length inputT) 1) )
      :for line in inputT
      :for n :from 0
      :collect  (rot-left n (append  padding line)
        ))))



(print *test_data*)
(print (rot-45 (print *test_data*)))


(print (rot+45 (print *test_data*)))

(find-xmas (rot-45 *test_data*))
;
(defun find-xmas (input)
  (let* ((xmas (str-to-list "XMAS"))
         (samx (reverse xmas)) 
         (tokens (list xmas samx))
         )
    (loop :for rotated-direction :in (list input (rotate input) (rot+45 input) (rot-45 input))     
     :sum ( loop
                  :for line
                  :in rotated-direction
                  :sum ( loop :for token :in tokens
                    :sum ( loop 
                        :for start = 0 :then (+ 1 start)
                        :while (setq start (search token line :start2 start))
                        :count t
                  )))
)))


(find-xmas (read-input "2024/day4/test_input"))
(find-xmas (read-input "2024/day4/input"))

(parse (str-to-list  "mul(123,123)") nil)
(parse (str-to-list  *test_data*) nil)
(parse (str-to-list  *test_data2*) nil t t)
(parse (read-input2 "2024/day4/test_input") nil)

(parse (read-input2 "2024/day4/input") nil)
(parse (read-input2 "2024/day4/input") nil t t)


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









