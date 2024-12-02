(defvar *test-input* ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(defun char-2-coord (c)
    (cond 
     ((char-equal #\> c) '(0 1))
     ((char-equal #\< c) '(0 -1))
    )
)

(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :collect riv )))

(defun to-charlist (s) (coerce s 'list))

(defun circular (items)
        ( let ((items-copy (copy-list items)))
            (setf (cdr (last items-copy )) items-copy)
        items-copy
        ))

(defun move-rock (rock offset-coord)
    (mapcar (lambda (c) (mapcar '+ c offset-coord)) rock)
    )


(defun get-rocks ()
    '(
      ((0 0)(0 1)(0 2)(0 3))
      ((2 1)(1 0)(1 1)(1 2)(0 1))
      ((0 0)(0 1)(0 2)(1 2)(2 2))
      ((3 0)(2 0)(1 0)(0 0))
      ((1 0)(1 1)(0 0)(0 1))
      )
)

(defun print-map (array)
  (terpri)
  (loop for i from 0 below (array-dimension array 0)
        do (loop for j from 0 below (array-dimension array 1)
                 do (princ (aref array i j))
                 (if (= j (1- (array-dimension array 1)))
                     (terpri)))))

(defun print-rock (rock)
    (let ((canvas (make-array '(7 7))))
     (dolist (coord rock)
         (setf (aref canvas (first coord) (second coord)) 4)
     )
     (print-map canvas))
)


(defun print-rocks (rocks)
    (dolist (rock rocks) (print-rock rock) (format t "-------"))
    )

(print-rocks (mapcar (lambda(r) (move-rock r '(0 2))) (get-rocks)))


(defun move-wind (rock coord floor)
    (let* (
           (moved-rock (move-rock rock coord))
           (x-coords (mapcar 'second moved-rock)))        
        (cond 
         ((< (apply 'min x-coords) 0) rock)
         ((> (apply 'max x-coords) 6) rock)
         ((colission moved-rock floor) rock)
         (t moved-rock)
         )        
    ))

;(let ((l (circular (to-charlist *test-input*))))
;    (dotimes (n 300)
;        (print (pop l))
;        )
;    )


(defun calc-height (floor)
    ;(print (list :floor floor))
    (apply 'max (mapcar 'first floor)))

(defun move-rock-to-start (rock height)
    (move-rock rock (list (+ 4 height) 2))
    )

(defun colission (rock floor)
    (find-if (lambda(c) (find c floor :test 'equal)) rock))

(defun update-floor (floor rock)
    (loop for (f-y f-x) in floor
          for candidates = (mapcar 'first (remove-if-not (lambda (r-coord) (= f-x (second r-coord))) rock))
          collect `( ,(apply 'max (cons f-y candidates)) , f-x)
            ))

(defun simulate (input iterations)
    (loop
     :with floor = '((0 0)(0 1)(0 2)(0 3)(0 4)(0 5)(0 6))
     :with winds = (circular (mapcar 'char-2-coord (to-charlist input)))
     :with rocks = (circular (get-rocks))            
     :for height = (calc-height floor)
     :for rock = (move-rock-to-start (pop rocks) height)      
     :for n :from 0 :below iterations 
     :do ( setf floor 
         (update-floor floor 
                       (do* ((current-rock (move-wind rock (pop winds) floor) (move-wind next-rock (pop winds) floor))
                             (next-rock (move-rock current-rock '(-1 0)) (move-rock current-rock '(-1 0)))
                             (n 0 (1+ n))) 
                               ((colission next-rock floor) current-rock)
                           ;(print (list n current-rock))
                           ;(when (< 10 n ) (error "max iter"))
                           ))
         )
     ;:do (print (list :floor floor))
     :finally (return floor)
    ))

(mapcar 'first (simulate *test-input* 2022))

;(loop for n from 0 below 1 do (print "hello"))



(ql:quickload "fiveam")

(fiveam:def-suite 17am-suite)
(fiveam:in-suite 17am-suite)


(fiveam:test test-process
    ;(fiveam:is (= 1651 (first (process-bb (parse-file "day16/test") 30 1))))    
)

(fiveam:run! '17am-suite)