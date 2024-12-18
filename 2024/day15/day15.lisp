(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun parse-input (file)
  (with-open-file (in file)
  (list
     (list-to-2d-array (loop          
                  :for riv = (read-line in nil)                   
                  :while (> (length riv) 0)                  
                  :collect (str-to-list riv) ))
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :append (str-to-list riv) ))    
    ))

(defun load-map (file)
(list-to-2d-array (read-input file)))


(defmacro aref-2d (array indices)
  `(aref ,array (first ,indices) (second ,indices)))


(defun load-orign (mapa)
  (loop :named outer
        :with rows := (array-dimension mapa 0)
        :with cols := (array-dimension mapa 1) 
                   :for x :from 0 :below rows
                    :do (loop :for y :from 0 :below cols
                    :when (eq (aref mapa x y) #\@)
                      :do (return-from outer (list x y))
            )))




(defun move (direction coord mapa)  
  (let* ((target-coord (mapcar #'+ direction coord)))
  (if (cond
    ((eq (aref-2d mapa target-coord) #\#) nil)
    ((eq (aref-2d mapa target-coord) #\.) t )
    ((eq (aref-2d mapa target-coord) #\O) (not (equalp (move direction target-coord mapa) target-coord)))
  )
    (progn (setf (aref-2d mapa target-coord) (aref-2d mapa coord) )
           (setf (aref-2d mapa coord) #\. )
            target-coord)
    coord
  )))


(defun direction->coord (c)
  (cond
   ((eq c #\^) '(-1 0))
   ((eq c #\v) '(1 0))
   ((eq c #\<) '(0 -1))
   ((eq c #\>) '(0 1))
   ))

(defun array-to-list (array)
  (loop :for i :below (array-dimension array 0)
        :collect (loop :for j :below (array-dimension array 1)
                       :collect (aref array i j))))


(defun solve-first (file)
  (destructuring-bind (mapa moves) (parse-input file)
      (let ((first-coord (load-orign mapa)))
      (format t "Initial~%~{~{~a~}~%~}" (array-to-list mapa))
      (loop :for c :in moves
        :for direction := (direction->coord c)
        :for coord := (move direction first-coord mapa) :then (move direction coord mapa)
        ;:do (format t "move ~a~%~{~{~a~}~%~}" c (array-to-list mapa))
        :finally (return (calculate-gps mapa))
      ))
  ))


(defun calculate-gps (mapa)
  (loop
        :with rows := (array-dimension mapa 0)
        :with cols := (array-dimension mapa 1) 
                   :for x :from 0 :below rows
                    :sum (loop :for y :from 0 :below cols
                    :when (eq (aref mapa x y) #\O)
                      :sum (+(* 100 x) y)
            )))

(solve-first "2024/day15/test_input0")

(solve-first "2024/day15/test_input1")
(solve-first "2024/day15/input")


; (solve-first (read-input "2024/day15/test_input0") '(7 11) )
; (solve-second (rec 3ad-input "2024/day15/test_input0") '(7 11) )

; (solve-first (read-input "2024/day15/input" ) '(103 101))
;(solve-second (read-input "2024/day15/input" ) '(103 101))

; (solve-second (read-input "2024/day15/test_input0"))
; (solve-second (read-input "2024/day15/input"))



