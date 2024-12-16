(ql:quickload "cl-ppcre")

(defun read-input (file)
  (with-open-file (in file)
     (loop          
                  :for Ariv = (read-line in nil)
                  :for Briv = (read-line in nil)
                  :for Criv = (read-line in nil)
                  :for Driv = (read-line in nil)                  
                  :while Criv                  
                  :collect 
     (list 
                      (list
              (mapcar (lambda (s) (parse-integer (second (cl-ppcre:split "(\\+)" s)))) (cddr (cl-ppcre:split "(,?\\s+)" Ariv)))
              (mapcar (lambda (s) (parse-integer(second (cl-ppcre:split "(\\+)" s)))) (cddr (cl-ppcre:split "(,?\\s+)" Briv))))
             (mapcar (lambda (s) (parse-integer (second (cl-ppcre:split "(=)" s)))) (cdr (cl-ppcre:split "(,?\\s+)" Criv)))
 ))))


(read-input "2024/day13/test_input0")

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))



(defun inverse (a b c d)
  (let ((det (- (* a d)(* b c))))
    (list (list (/ d det) (/ (- b) det)) (list (/ (- c) det) (/ a det))) 
  ))

(defun solve-first (machines)
  (loop 
   :for (((a b) (c d)) xy) in machines
   :for answer := (mapcar (lambda(row) (apply #'+ (mapcar #'* row xy)) ) (inverse a c b d))
   :when (every #'integerp answer)
   :sum  (apply #'+ (mapcar #'* answer '(3 1)))
   )
  )


(defun solve-second (machines)
  (loop 
   :for (((a b) (c d)) xy) in machines
   :for answer := (mapcar (lambda(row) (apply #'+ (mapcar #'* row  (mapcar (lambda(f) (+ f 10000000000000)) xy)))) (inverse a c b d))
   :when (every #'integerp answer)
   :sum  (apply #'+ (mapcar #'* answer '(3 1)))
   )
  )

(solve-first (read-input "2024/day13/test_input0"))

(solve-first (read-input "2024/day13/input"))

(solve-second (read-input "2024/day13/test_input0"))
(solve-second (read-input "2024/day13/input"))


