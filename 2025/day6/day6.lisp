(ql:quickload "cl-ppcre")

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :append (mapcar #'(lambda (x) (mapcar #'identity (cl-ppcre:split "\\s+" (string-trim '(#\Space) x)))) (cl-ppcre:split "," riv ));(mapcar #'parse-integer (cl-ppcre:split "," riv ))
  )))


(mapcar #'1+ '(1 2 3))

(defun problem1 (path) 
      (let* ((data (reverse (read-input path)))
             (operators ( mapcar #'(lambda (x) (symbol-function (intern x))) (car data)))
             (numbers (mapcar #'(lambda (x) (mapcar #'parse-integer x)) (cdr data)) )
             )
            (print operators)
            (print numbers)

            (apply '+ (reduce #'(lambda (x y) (mapcar #'(lambda (xn yn op) (funcall op xn yn)) x y operators)) numbers ))
            )
      )

(funcall (symbol-function (intern "+")) 1 1)


(problem1 "2025/day6/test_input")
(problem1 "2025/day6/input")


(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (apply #'max (mapcar #'length  list)))
              :initial-contents list))

(defun read-input2 (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (str-to-list riv) )))

(defun remove-spaces (c)
      ( remove-if #'(lambda (x) (or (not x) (eq x #\Space))) c)
      )

(defun problem2 (path) 
(loop 
 with full :=  (read-input2 path)
 with operators := (print (mapcar  #'(lambda (x) (symbol-function (intern (string x)))) ( remove-spaces (car (reverse full)))))
 with buff := nil
 for numbers := (reverse (cdr (reverse full))) :then  (mapcar #'cdr numbers)
 for c :=   (mapcar #'car numbers)
 for cclean := (remove-spaces c)
:do (print (list "prebuff" buff))
 :if  (not cclean)
      :sum ( let ((aux (print (apply (pop operators) (print buff)))))
            (setf buff '())
            aux
      )
 :else
      :do (push (loop
            with acc := 0 
            for n in (print cclean)
            for e := 1 :then (* e 10)
            :until (or (not n) (eq n #\Space))
            :do (setf acc (+ (* 10 acc) (- (char-code n) (char-code #\0)))) 
            :finally (return acc)
      ) buff )

      :while (some #'identity c)
      )
)

(problem2 "2025/day6/test_input")




(problem2 "2025/day6/input")