(ql:quickload "cl-ppcre")

(defun read-input (file)
  (with-open-file (in file)
     (loop          
                  :for riv = (read-line in nil)
                  :while riv
                  :collect (mapcar (lambda (s) (reverse (mapcar #'parse-integer (cl-ppcre:split "," s)))) (cdr (cl-ppcre:split "(\\s?\\w=)" riv)))
                  ;#:collect (mapcar (lambda (s) (parse-integer (second (cl-ppcre:split "(\\+)" s)))) (cddr (cl-ppcre:split "(,?\\s+)" Ariv)))
          
 )))


(read-input "2024/day14/test_input0")

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defmacro aref-2d (array indices)
  `(aref ,array (first ,indices) (second ,indices)))

(defun generate-map (dimensions positions)
  (loop
   :with mapa := (make-array dimensions :element-type 'integer)
   :for pos in positions
    :do (incf (aref-2d mapa pos))
   :finally (return mapa)
   )  )



(defun evolve (robots seconds dimensions)
  (mapcar (lambda (robot) (mapcar (lambda (p v d) (mod (+ p (* v seconds)) d)) (first robot) (second robot)  dimensions )) robots)
  )


(defun transform-map (mapa)
  (loop
   :with pretty := (make-array (array-dimensions mapa) :initial-element '\-   )
   :for x :from 0 :below (array-dimension mapa 0)
   :do (loop :for y :from 0 :below (array-dimension mapa 1)
        :do (when (> (aref mapa x y ) 0) (setf (aref pretty x y) '@))
             )
  :finally (return pretty))
  )

(defun count-cuadrants (mapa dim)
  (loop
    :with mid := (mapcar (lambda (x) (floor x 2)) dim)
    :for (x0 x1) in (list
      (list 0 (first mid))
      (list (1+ (first mid)) (first dim)))
    :append (loop :for (y0 y1) :in (list
              (list 0 (second mid))
              (list (1+ (second mid)) (second dim)))
            :collect( loop :for x :from x0 :below x1
                                  :sum (loop :for y :from y0 :below y1
                                                  :sum (aref mapa x y)
                                                  )))))

(defun solve-first (robots dim)
  (apply #'* (count-cuadrants (generate-map dim (evolve robots 100 dim )) dim )))


(defun solve-second (robots dim)
  (setf *print-right-margin* 40)
  
  (loop :for x :from 0 :below 10000
        :for mapa :=  (generate-map dim (evolve robots x dim ))
        :when t; (search-tree mapa)
        :do (progn (print x)
            (print (transform-map mapa))
            (sleep 0.5))
  )
  )



(defun search-tree (mapa)
  (loop
   :for x :from 0 :below (floor (array-dimension mapa 0) 2)
   :for cnt := (loop :for y :from 0 :below (array-dimension mapa 1)
        :sum (aref mapa x y) )
   :until (> cnt 2)   
   :finally (return (not (> cnt 2)))
  ))

(solve-first (read-input "2024/day14/test_input0") '(7 11) )
(solve-second (read-input "2024/day14/test_input0") '(7 11) )

(solve-first (read-input "2024/day14/input" ) '(103 101))
(solve-second (read-input "2024/day14/input" ) '(103 101))

(solve-second (read-input "2024/day14/test_input0"))
(solve-second (read-input "2024/day14/input"))


