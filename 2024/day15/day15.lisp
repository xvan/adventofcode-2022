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

(defun clone-2d-array (array)
  (let ((rows (array-dimension array 0))
        (cols (array-dimension array 1)))
    (make-array (list rows cols)
                :initial-contents (loop :for i :below rows
                                        :collect (loop :for j :below cols
                                                       :collect (aref array i j))))))
      
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

(defun calculate-gps (mapa)
  (loop
        :with rows := (array-dimension mapa 0)
        :with cols := (array-dimension mapa 1) 
                   :for x :from 0 :below rows
                    :sum (loop :for y :from 0 :below cols
                    :when (or (eq (aref mapa x y) #\O) (eq (aref mapa x y) #\[))
                      :sum (+(* 100 x) y)
            )))

(defun wider (line) (apply #'append (mapcar (lambda (c)
          (cond
           ((eq c #\#) '(#\# #\#))
           ((eq c #\O) '(#\[ #\]))
           ((eq c #\.) '(#\. #\.))
           ((eq c #\@) '(#\@ #\.))           
           )
          ) line)))


(defun parse-input2 (file)
  (with-open-file (in file)
  (list
     (list-to-2d-array (loop          
                  :for riv = (read-line in nil)                   
                  :while (> (length riv) 0)                  
                  :collect (wider (str-to-list riv)) ))
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :append (str-to-list riv) ))    
    ))



(defun savetxt (filename mapa step direction enable-fo)
  (when enable-fo
  (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "step~a:~a~%~{~{~a~}~%~}" step direction (array-to-list mapa))    
  ))
  (format nil "step~a:~a~%~{~{~a~}~%~}" step direction (array-to-list mapa))
  )


(defun step-many (step-pairs mapa)
  (loop :with new-mapa := (clone-2d-array mapa)
        :for (origin target) :in step-pairs
        :for current-tile := (aref-2d new-mapa origin)
        :do 
          (setf (aref-2d new-mapa target) current-tile) 
          (setf (aref-2d new-mapa origin) #\.)
        :finally (return new-mapa)              
        )
  )

(defun clear-many (direction targets mapa)
  (loop :for target :in targets
              :for (status loop-mapa) := (tile-clear direction target mapa) :then (tile-clear direction target loop-mapa)
              :while status
              :finally (return (list status loop-mapa))
            )
  )

(defun tile-clear (direction coord mapa)
  (let* ((current-tile (aref-2d mapa coord)))
    (cond 
     ((eq current-tile #\.) (list t mapa))
     ((eq current-tile #\#) (list nil mapa))
     ((eq current-tile #\O) (tile-move direction coord mapa))
     ((eq current-tile #\[) (tile-move direction coord mapa))
     ((eq current-tile #\]) (tile-move direction coord mapa))
     )
    )
  )

         

(defun tile-move (direction coord mapa)
    (let* (
         (current-tile (aref-2d mapa coord))
         (target-coord (mapcar #'+ direction coord))         
         (right-target-coord (mapcar #'+ target-coord '(0 1)))
         (left-target-coord (mapcar #'- target-coord '(0 1)))
         (right-coord (mapcar #'+ coord '(0 1)))
         (left-coord (mapcar #'- coord '(0 1)))
         (width-movement (= (first direction) 0))
         (height-movement (not width-movement))
         (clear-targets (cond 
          ((eq current-tile #\@) (list target-coord))
          ((eq current-tile #\O) (list target-coord))
          ((and (eq current-tile #\[) width-movement) (list target-coord))
          ((and (eq current-tile #\]) width-movement) (list target-coord))
          ((and (eq current-tile #\[) height-movement) (list target-coord right-target-coord))
          ((and (eq current-tile #\]) height-movement) (list left-target-coord target-coord))))
         (step-pairs (cond 
          ((eq current-tile #\@) (list (list coord target-coord)))
          ((eq current-tile #\O) (list (list coord target-coord)))
          ((and (eq current-tile #\[) width-movement) (list (list coord target-coord)))
          ((and (eq current-tile #\]) width-movement) (list (list coord target-coord)))
          ((and (eq current-tile #\[) height-movement) (list (list coord target-coord) (list right-coord right-target-coord)))
          ((and (eq current-tile #\]) height-movement) (list (list left-coord left-target-coord) (list coord target-coord))))))
    (destructuring-bind (clear-status cleared-map) (clear-many direction clear-targets mapa )
      (list clear-status 
            (if clear-status (step-many step-pairs cleared-map) mapa)
            (if clear-status target-coord coord)            
            ) 
      )
    )
  )

(defun solve-tiles (file parser enable-fo)
  (destructuring-bind (mapa moves) (funcall parser file)
      (let ((first-coord (load-orign mapa)))      
      (loop 
            :with frame := 0
            :for c :in moves
            :for old-print := (savetxt (format nil "/tmp/mapas/~5,'0d.txt" (incf frame))  mapa x c enable-fo) :then (savetxt (format nil "/tmp/mapas/~5,'0d.txt" (incf frame))  new-map x c enable-fo)
            :for x :from 0            
        :for direction := (direction->coord c)
        :for (status new-map new-coord) := (tile-move direction first-coord mapa) :then (tile-move direction new-coord new-map)               
       :for new-print := (savetxt (format nil "/tmp/mapas/~5,'0d.txt" (incf frame))  new-map x c enable-fo)
      ;  :do   (print old-print)
      ;         (print new-print)              
        :finally (return (calculate-gps new-map))
      ))
  ))


; (untrace tile-move)
; (untrace tile-clear)
; (untrace clear-many)
; (untrace step-many)

(time (solve-tiles "2024/day15/test_input0" #'parse-input nil ))
(time (solve-tiles "2024/day15/test_input1" #'parse-input nil ))
(time (solve-tiles "2024/day15/input" #'parse-input nil))
(time (solve-tiles "2024/day15/test_input1" #'parse-input2 nil))
(time (solve-tiles "2024/day15/input" #'parse-input2 nil))