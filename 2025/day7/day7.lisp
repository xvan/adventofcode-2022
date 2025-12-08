(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun array-to-list (array)
  (loop :for i :below (array-dimension array 0)
        :collect (loop :for j :below (array-dimension array 1)
                       :collect (aref array i j))))


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



(defun problem1 (path)
      (loop 
             :with mapa := (load-map path)
             :with dimensions :=  (array-dimensions mapa)
             :for x :from 1 :below (first dimensions)
             :sum (loop
                  :for y :from 0 :below (second dimensions)
                  :for current := (aref mapa x y)
                  :for parent := (aref mapa (1- x) y)
                  :count (and (eq parent #\|) (eq current #\^))
                  :when (or (eq parent #\|) (eq parent #\S))
                  :do (case current 
                        ( #\^ 
                         (when ( <= 0 (1- y)) (setf (aref mapa x (1- y)) #\|) )
                         (when ( > (second dimensions) (1+ y)) (setf (aref mapa x (1+ y)) #\|) )
                        )
                        ( #\S nil)
                        ( otherwise (setf (aref mapa x y) #\|))
                       ))             
             ;:do (print mapa)
             ))

(problem1 "2025/day7/test_input")
(problem1 "2025/day7/input")


(defun ray-trace (mapa dimensions x y)
      (if (>= (1+ x) (first dimensions)) 
          1
          (case (aref mapa (1+ x) y)
                ( #\^ 
                        (+
                         (if ( <= 0 (1- y)) (ray-trace mapa dimensions (1+ x) (1- y)) 0)
                         (if ( > (second dimensions) (1+ y)) (ray-trace mapa dimensions (1+ x) (1+ y)) 0) 
                ))
                (otherwise (ray-trace mapa dimensions (1+ x) y))
            )
      )
) 
       
(defun problem2 (path)
      (let* ((mapa (load-map path))
             (dimensions (array-dimensions mapa))
             (startcol (loop for y from 0 below (second dimensions) :when (eq #\S (aref mapa 0 y)) :return y)))
            (ray-trace mapa dimensions 0 (print startcol))
      )                      
) 

(defun fill-zeros (mapa)
      (loop 
       :for x :from 0 :below (array-dimension mapa 0)
       :do (loop :for y :from 0 :below (array-dimension mapa 1)
                            :when (eq #\. (aref mapa x y)) 
                            :do (setf (aref mapa x y) 0)
                            :when (eq #\S (aref mapa x y)) 
                            :do (setf (aref mapa x y) 1)
      )
      )
      mapa
      )

(fill-zeros (load-map "2025/day7/test_input"))

(defun print-map (mapa)
      (format t "~{~{~a~}~%~}"  (array-to-list mapa))
      )
(defun problem2b (path)
      (loop 
             :with mapa := (fill-zeros (load-map path))
             :with dimensions :=  (array-dimensions mapa)
             :for x :from 1 :below (first dimensions)
             :do (loop
                  :for y :from 0 :below (second dimensions)
                  :for current := (aref mapa x y)
                  :for parent := (aref mapa (1- x) y)
                  :when (and (numberp parent) (> parent 0))
                  :do (case current 
                        ( #\^ 
                         (when ( <= 0 (1- y)) (incf (aref mapa x (1- y)) parent) )
                         (when ( > (second dimensions) (1+ y)) (incf (aref mapa x (1+ y)) parent) )
                        )
                        ( #\S nil)
                        ( otherwise (incf (aref mapa x y) parent))
                       ))
             ;:do (print-map mapa)
             ;:do (terpri)
             :finally (return (loop :for x :from 0 :below (second dimensions) :sum (aref mapa (1- (first dimensions)) x )))
             ))


 (problem2b "2025/day7/test_input")

(problem2b "2025/day7/input")