(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (str-to-list riv) )))



(defun load-map (file)
(list-to-2d-array (read-input file)))

(defun generate-neighbours (coord dimensions)      
      (let ((delta '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))))
      (remove-if             
            #'(lambda (n) (some #'identity (mapcar #'(lambda (nc dc) (or (< nc 0) (>= nc dc))) n dimensions)))                              
            (mapcar #'(lambda (d) (mapcar #'+ coord d)) delta))      
      )
      )


(defun problem1 (path)
      (let* (
                   (mapa (load-map path))
                   (dimensions (array-dimensions mapa))
                   )
      (loop 
             :for x :from 0 :below (first dimensions)             
             :sum (loop
                   :for y :from 0 :below (second dimensions)
                   :for nb-cnt := (loop
                         :for nb :in (generate-neighbours (list x y) dimensions)
                         :count (eq (aref mapa (first nb) (second nb))  #\@ )  
                         )
                   :when (eq (aref mapa x y)  #\@ )  
                   :count (< nb-cnt 4)
                   ;collect (list x y nb-cnt (generate-neighbours (list x y) dimensions))
                   )             
             )
      ))

(problem1 "2025/day4/test_input")
(problem1 "2025/day4/input")


(defun problem2 (path)
      (let* (
                   (mapa (load-map path))
                   (dimensions (array-dimensions mapa))
                   )
            
      (loop
             :for step-cnt :=
                  (loop 
                        :for x :from 0 :below (first dimensions)             
                        :sum (loop
                              :for y :from 0 :below (second dimensions)
                              :for nb-cnt := (loop
                                    :for nb :in (generate-neighbours (list x y) dimensions)
                                    :count (eq (aref mapa (first nb) (second nb))  #\@ )  
                                    )
                              :when (eq (aref mapa x y)  #\@ )
                              :count (if (< nb-cnt 4) (setf (aref mapa x y) #\x) nil)
                              ;collect (list x y nb-cnt (generate-neighbours (list x y) dimensions))
                              ))
             :for step :from 1
             :while (> step-cnt 0 )
             :do (print (list step step-cnt))
             :sum step-cnt
             )))

(problem2 "2025/day4/test_input")

(problem2 "2025/day4/input")