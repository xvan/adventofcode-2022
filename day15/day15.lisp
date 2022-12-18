(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :collect riv )))

(defun split-by-string (separator string)    
    (loop with l = (length separator)
          for i = 0 then (+ j l)
          as j = (search separator string :start2 i)          
          collect (subseq string i j)
          while j))


(defun get-between (string start-index start-token end-token)
    
    (let*
            (
             (start-token-length (length start-token))             
             (i (+ (search start-token string :start2 start-index) start-token-length))
             (j (if end-token (search end-token string :start2 i) (length string)))
             )
             (list i j)
             )        
        )

(defun parse-line (string)
    (loop 
     with n = 0
     for (start-token end-token) in  '(("x=" ",") ("y=" ":") ("x=" ",") ("y=" nil))
     for (start stop)=(get-between string n start-token end-token)    
     do (setf n stop)
     collect (parse-integer string :start start :end stop) into outputs
     finally (return (destructuring-bind (s-x s-y b-x b-y) outputs `((,s-x ,s-y)(,b-x ,b-y))))
    )               
)




(defun parse-lines (file) (mapcar 'parse-line (read-input file)))


(defun generate-map (lines)    
    (destructuring-bind ((min-s-x max-s-x)(min-s-y max-s-y)) (minmax (mapcar 'first lines))
            (let* 
                    (
                     (max-d (calc-max-distance lines))
                     (max-y  (+ max-s-y max-d))
                     (min-y  (- min-s-y max-d))
                     (max-x  (+ max-s-x max-d))
                     (min-x  (- min-s-x max-d))
                    )
                `( :map ,(make-array (list (1+ (- max-y min-y)) (1+ (- max-x min-x)))) :offset (,min-x ,min-y))
            )
        
        )
    )

(defun calc-max-distance (lines)
    (apply 'max 
        (mapcar (lambda (l) (apply 'calc-distance l))  lines )))




(defun calc-distance (a b)
    (apply '+ (mapcar 'abs (mapcar '- a b)))
    )

(defun minmax (lines)
    (loop for (x y) in lines
              :maximize x into max-x
              :minimize x into min-x
              :maximize y into max-y
              :minimize y into min-y
              :finally (return `((,min-x ,max-x)  (,min-y ,max-y)))
    ))


(defun process (lines target-row)
    (let* (
           (map-data (generate-map lines))
           (map (getf map-data :map))
           (offset (getf map-data :offset))
           (correct-offset (lambda (c) (mapcar '- c offset)))
           )
        (loop 
         for (s-coord b-coord) in lines
         for distance = (calc-distance s-coord b-coord)
         for (c-x c-y) = (funcall correct-offset s-coord)
            do  (loop for x from (* -1 distance) to distance
                  do (loop for y from (* -1 (- distance (abs x))) to (- distance (abs x))
                           do (setf (aref map (+ c-y y) (+ c-x x)) 4)
                           )))
        (loop 
         for (s-coord b-coord) in (mapcar (lambda (l) (mapcar correct-offset l))lines)
         do (set-point map s-coord 5)
         do (set-point map b-coord 3)
         )
        (print-map map)


        (loop
         with y = (second (funcall correct-offset `(0 ,target-row)))
         for x from 0 below (array-dimension map 1)
         when (= 4 (aref map y x)) sum 1))
        )


(defun print-map (array)
  (terpri)
  (loop for i from 0 below (array-dimension array 0)
        do (loop for j from 0 below (array-dimension array 1)
                 do (princ (aref array i j))
                 (if (= j (1- (array-dimension array 1)))
                     (terpri)))))



(defun scan-row (lines target-y)
    (remove-duplicates
    (loop 
     for (sensor beacon) in lines
     for (s-x s-y) = sensor
     for beacon-distance = (calc-distance sensor beacon)
     for target-distance = (calc-distance sensor `(,s-x ,target-y))
     for changui = (- beacon-distance target-distance)
     ;do (print `(,sensor ,beacon ,s-x ,s-y ,beacon-distance ,target-distance ,changui))
     when (>= changui 0)              
        append (loop for x from (- s-x changui) to (+ s-x changui) collect x)
     )))

(defun process-b (lines target-y)
    (let ((filtered  (remove-duplicates (mapcar 'first (remove-if-not (lambda (c) (= target-y (second c))) (apply 'append lines)))) ))
        (remove-if (lambda (x) (member x filtered)) (scan-row lines target-y))
    ))


(defun make-tester (lines)
    (lambda (coord) 
        (loop :for tester :in 
                  (mapcar (lambda (l) 
                              (destructuring-bind (sensor beacon) l
                                  (let ((beacon-distance (calc-distance sensor beacon)))
                                      (lambda (c) (> (calc-distance sensor c) beacon-distance )))))
            lines)
                  
            :when (not (funcall tester coord)) :do (return nil)
            :finally (return t)
         )))


(defun process-c (lines range)
    (loop
        :with tester = (make-tester lines) 
     :for x :from 0 :to range
     :for target = (
        loop for y :from 0 :to range
             for coord = (list x y)
             when (funcall tester coord) :do (return coord)              
      )
     :when target :do (return target)
    )
)


(defun coord-to-freq (coord)
    (destructuring-bind (x y) coord
         (+ (* x 4000000) y)))



;(process-d (parse-lines "day15/test"))

;(process (parse-lines "day15/test") 20)

;(parse-lines "day15/test")

;(calc-max-distance (parse-lines "day15/input"))
;(parse-lines "day15/test")
;(process (parse-lines "day15/test") 10)

;(length (process-b (parse-lines "day15/input") 2000000))

(defmacro set-point (map coord value)
    `(destructuring-bind (x y) ,coord
        (setf (aref ,map y x) ,value)
        )
    )


(defun generate-sub-canvas (canvas box)
    (destructuring-bind (((min-c-x max-c-x)(min-c-y max-c-y)) ((min-b-x max-b-x)(min-b-y max-b-y))) (list canvas box)
        `(
                            ((,min-c-x ,(min max-c-x min-b-x))(,min-c-y ,(min max-c-y min-b-y)))
                            ((,min-c-x ,(min max-c-x min-b-x))(,(max min-c-y min-b-y) ,(min max-c-y max-b-y)))
                            ((,min-c-x ,(min max-c-x min-b-x))(,(max min-c-y max-b-y) ,max-c-y))

                            ((,(max min-c-x min-b-x) ,(min max-c-x max-b-x))(,min-c-y ,(min max-c-y min-b-y)))
                            ((,(max min-c-x min-b-x) ,(min max-c-x max-b-x))(,(max min-c-y max-b-y) ,max-c-y))

                            ((,(max min-c-x max-b-x) ,max-c-x)(,min-c-y ,(min max-c-y min-b-y)))
                            ((,(max min-c-x max-b-x) ,max-c-x)(,(max min-c-y min-b-y) ,(min max-c-y max-b-y)))
                            ((,(max min-c-x max-b-x) ,max-c-x)(,(max min-c-y max-b-y) ,max-c-y)))))

(defun crop-canvas (canvas box)            
        (remove-if-not 
                (lambda (box) 
                    (destructuring-bind ((min-x max-x)(min-y max-y)) box 
                        (and (< min-x max-x) (< min-y max-y))))
                (generate-sub-canvas canvas box)))


(defun limits (lines)
    (loop      
     for (sensor beacon) in lines
     for (s-x s-y) = sensor
     for d = (calc-distance sensor beacon)
     collect `((,(- s-x d) ,s-y ) (,(+ s-x d) ,s-y ) (,s-x ,(- s-y d)) (,s-x ,(+ s-y d)))
    )
)



(defun rotate (coord)
    (destructuring-bind (x y) coord
        `( ,(+ x y) ,(- x y) )
    )
)

(defun anti-rotate (coord)
    (destructuring-bind (a b) coord
        `( ,(/ (+ a b) 1) ,(/ (- a b) 1) )
    )
)

(defun rotate-all (coords)
    (mapcar (lambda (l) (mapcar 'rotate l)) coords))


(defun anti-rotate-all (coords)
    (mapcar (lambda (l) (mapcar 'anti-rotate l)) coords))


(defun crop-all-canvas (box canvas)
    (mapcan (lambda(c) (crop-canvas c box)) canvas ))

(defun process-d (lines)
    (let* (
           (rotated (rotate-all (limits lines)))           
           )           
        (do (
             (boxes (mapcar 'minmax rotated) (cdr boxes) ) 
             (canvas (list (minmax (apply 'append rotated))) (crop-all-canvas (car boxes) canvas))
            )
            ((null boxes) canvas)
)))


(defun process-e (lines)
    (let* (
           (rotated (rotate-all (limits lines)))
           (target-r (rotate '(14 11)))
           )           
        (do (
             (boxes (mapcar 'minmax rotated) (cdr boxes) ) 
             (canvas (list (minmax (apply 'append rotated))) (crop-all-canvas (car boxes) canvas))
            )
            ((null boxes) canvas)
            (print (reduce (lambda (a b) (and a b)) (mapcar (lambda (c) (contains c target-r)) canvas)))
            (print (car boxes))
            (print target-r)
)))


(process-e (parse-lines "day15/test"))

(defun contains (box coord)
     (destructuring-bind (((min-b-x max-b-x)(min-b-y max-b-y)) (c-x c-y)) (list box coord)
        (and (<= min-b-x c-x) (<= c-x max-b-x) (<= min-b-y c-y) (<= c-y max-b-y))
    ))

(anti-rotate-all (process-d (parse-lines "day15/test")))

(crop-all-canvas '((0 80)(0 80)) (anti-rotate-all (process-d (parse-lines "day15/test"))))


(ql:quickload "fiveam")

(fiveam:def-suite 15am-suite)
(fiveam:in-suite 15am-suite)

(fiveam:test distance
    (fiveam:is (= 0 (calc-distance '(1 1) '(1 1))))
    (fiveam:is (= 1 (calc-distance '(0 0) '(0 1))))
    (fiveam:is (= 1 (calc-distance '(0 0) '(0 -1))))
    (fiveam:is (= 2 (calc-distance '(0 0) '(1 -1))))
    (fiveam:is (= 2 (calc-distance '(2 2) '(3 3))))
    )

(fiveam:test parsing
    (fiveam:is (equal '((2 18)(-2 15)) (parse-line "Sensor at x=2, y=18: closest beacon is at x=-2, y=15")))
    )

(fiveam:test test-process 
    (fiveam:is (equal 26 (process (parse-lines "day15/test") 10)))
    (fiveam:is (equal 26 (length (process-b (parse-lines "day15/test") 10))))
    ;(fiveam:is (equal 5335787 (length  (process-b (parse-lines "day15/input") 2000000))))
    
    ;(fiveam:is (equal 56000011 (coord-to-freq (process-c (parse-lines "day15/test") 20))))
    ;(fiveam:is (equal 56000011 (coord-to-freq (process-c (parse-lines "day15/input") 4000000))))
    ;(fiveam:is (equal 5335787 (length  (process-b (parse-lines "day15/input") 2000000))))
    )

(fiveam:test test-crop-canvas
    (fiveam:is (equal nil (crop-canvas '((2 3)(2 3)) '((0 4)(0 4))))) ;Crop Covers All
    (fiveam:is (equal nil (crop-canvas '((2 3)(2 3)) '((2 3)(2 3))))) ;Matched
    (fiveam:is (equal '(((0 2) (0 2)) ((0 2) (2 3)) ((0 2) (3 4)) ((2 3) (0 2)) 
                                      ((2 3) (3 4)) ((3 4) (0 2)) ((3 4) (2 3)) ((3 4) (3 4))) 
                      (crop-canvas '((0 4)(0 4)) '((2 3)(2 3))))) ;Crop in the middle
    (fiveam:is (equal '(((2 4) (0 4))) (crop-canvas '((0 4)(0 4)) '((-1 2)(0 4))))) ;Crop left side
    (fiveam:is (equal '(((0 2) (0 4))) (crop-canvas '((0 4)(0 4)) '((2 5)(0 4))))) ;Crop right side
    (fiveam:is (equal '(((0 4) (2 4))) (crop-canvas '((0 4)(0 4)) '((0 4)(-1 2))))) ;Crop up
    (fiveam:is (equal '(((0 4) (0 2))) (crop-canvas '((0 4)(0 4)) '((0 4)(2 5))))) ;Crop down
    (fiveam:is (equal '(((0 2) (0 2)) ((0 2) (3 4)) ((2 4) (0 2)) ((2 4) (2 3)) ((2 4) (3 4))) (crop-canvas '((0 4)(0 4)) '((0 2)(2 3))))) ;Crop Middle Left
    (fiveam:is (equal '(((0 4) (0 2)) ((0 4) (3 4))) (crop-canvas '((0 4)(0 4)) '((-1 5)(2 3))))) ;split in two halves
    )


(fiveam:run! '15am-suite)