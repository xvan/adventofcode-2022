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

(defun parse-line (line)
        (mapcar (lambda (pair) (mapcar 'parse-integer (split-by-string "," pair))) (split-by-string " -> " line )))
            

(defun parse-lines (lines) (mapcar 'parse-line lines))

(defvar *test-data* '("498,4 -> 498,6 -> 496,6" "503,4 -> 502,4 -> 502,9 -> 494,9"))


(defun generate-sequence (a b)
    (loop for i from (min a b) to (max a b) collect i))


(defun expand-coords (lst)
    (mapcan 'line-coords lst (cdr lst)))

(defun process-lines (lines)
    (mapcan 'expand-coords lines)
    )

(defun minmax (lines)
    (loop for (x y) in (apply 'append lines)
              :maximize x into max-x
              :minimize x into min-x
              :maximize y into max-y
              :minimize y into min-y
              :finally (return `((,min-x ,max-x)  (,min-y ,max-y)))
    ))

(defun line-coords (start stop)
        (destructuring-bind ((start-x start-y) (stop-x stop-y)) (list start stop)
        (cond
         ((= start-x stop-x) (mapcar (lambda (y) (list start-x y)) (generate-sequence start-y stop-y)) )
         ((= start-y stop-y) (mapcar (lambda (x) (list x start-y)) (generate-sequence start-x stop-x)) )) ))

(defun ignore-binding (x)
    x)



(defmacro pair-ref (arr idx)
    `(apply #'aref (cons ,arr ,idx)))

(defun generate-map (lines)
    (destructuring-bind ((min-x max-x)  (min-y max-y)) (minmax lines)
        (ignore-binding min-y)
        (let (
              (my-map (make-array `(,(+  2 max-y) ,(+ 2 (1+ (- max-x min-x))))))
              (calc-x-offset (lambda(x) (1+ (- x min-x)) ))                        
              )
            (loop for (x y) in (process-lines lines)
                 
                 do (setf (aref my-map  y (funcall calc-x-offset x)) 1))
            `(:map ,my-map :start-coord (,(funcall calc-x-offset 500) 0)))
            ))
        
(loop for x from 0 below 10 collect x)

(defun generate-map-2 (lines)
    (destructuring-bind ((min-x max-x)  (min-y max-y)) (minmax lines)
        (ignore-binding min-y)
        (ignore-binding min-x)
        (ignore-binding max-x)
        (let* (
              (height (+  3 max-y))
              (my-map (make-array `(,height ,(1+ (* 2 height)) )))
              (calc-x-offset (lambda(x) (+ x height -500) ))                        
              )
            
            (destructuring-bind (h-y h-x) (array-dimensions my-map)
                (loop for x from 0 below h-x do (setf (aref my-map (1- h-y) x) 2 ) ))
            (loop for (x y) in (process-lines lines)                 
                 do (setf (aref my-map  y (funcall calc-x-offset x)) 1))
                    `(:map ,my-map :start-coord (,(funcall calc-x-offset 500) 0)))
            ))


(defun simulation (map-data)
    (let ((map (getf map-data :map))
           (start-coord (getf map-data :start-coord))
           )
         
        (do (
             (coord (step-map map start-coord) (step-map map start-coord))
             (prev-coord nil coord) 
             (n 0 (1+ n)))
            ((or (bottom-reached coord map) (equal prev-coord coord))  n)
            
            (destructuring-bind (x y) coord 
                 (setf (aref map y x) 8))
             ;(print map)             
             )))


(defun step-map (map start-coord)
    (do 
        ((coord start-coord (step-sand map coord))
         (prev-coord nil coord))
        ((or (equal coord prev-coord) (bottom-reached coord map)) coord))    
    )


(defun step-sand (map coord)
    (destructuring-bind (x y) coord        
        (cond
         ((= 0 (aref map (1+ y) x )) (list x (1+ y)) )
         ((= 0 (aref map (1+ y) (1- x) )) (list (1- x) (1+ y)) )
         ((= 0 (aref map (1+ y) (1+ x) )) (list (1+ x) (1+ y)) )         
         (t coord)
         )
        )    
    )

(defun bottom-reached (coord map)
    (= (second coord) (1- (array-dimension map 0))))

;(print (generate-map (parse-lines *test-data*)))

(print (generate-map-2 (parse-lines *test-data*)))

(generate-map-2 (parse-lines (read-input "day14/input")))
                             
(array-dimension (getf (generate-map (parse-lines *test-data*)) :map) 0)
(ql:quickload "fiveam")

(fiveam:def-suite 14am-suite)
(fiveam:in-suite 14am-suite)

(fiveam:test parsing
    (fiveam:is (equal '(((498 4) (498 6) (496 6)) ((503 4) (502 4) (502 9) (494 9))) (parse-lines *test-data*))) 
    )
 (minmax (parse-lines *test-data*))

(fiveam:test line-testing
    (fiveam:is (equal '((498 4) (498 5) (498 6)) (line-coords '(498 4) '(498 6) )))
    (fiveam:is (equal '((496 6) (497 6) (498 6)) (line-coords '(498 6) '(496 6) )))
    (fiveam:is  (equal '((498 4) (498 5) (498 6) (496 6) (497 6) (498 6)) (process-lines '(((498 4) (498 6)) ((498 6) (496 6))) )))

    (fiveam:is (equal '((494 503) (4 9)) (minmax (parse-lines *test-data*))))

    
    
    )

(fiveam:test test-process
    (fiveam:is (equal 24 (simulation (generate-map (parse-lines *test-data*))) ))
    (fiveam:is (equal 674 (simulation (generate-map (parse-lines (read-input "day14/input")))) ))
    (fiveam:is (equal 93 (simulation (generate-map-2 (parse-lines *test-data*))) ))
    (fiveam:is (equal 674 (simulation (generate-map-2 (parse-lines (read-input "day14/input")))) ))
    )

(fiveam:run! '14am-suite)