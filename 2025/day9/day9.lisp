(ql:quickload "cl-ppcre")


(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (mapcar #'parse-integer (cl-ppcre:split "," riv ))
  )))

(defun calculate-area (pair)
      (apply #'* (mapcar #'1+ (mapcar #'abs (mapcar #'- (first pair) (second pair))))))

(calculate-area '((2 5) (9 7)))

(defun distance (pair)
      (reduce #'+
       (mapcar #'(lambda (x) (* x x)) 
            (mapcar #'- (first pair) (second pair))
      )
      ))

(defun generate-pairs (coords)
      (mapcon #'(lambda (x) (mapcar #'(lambda (y) (list (car x) y)) (cdr x))) coords)
      )

(defun problem1 (path) ()
      (apply #'max 
      (mapcar #'calculate-area
      (generate-pairs (read-input path))      
      )
      ))


(problem1 "2025/day9/test_input")
(problem1 "2025/day9/input")      

 
(defun get-segments (data)
      (mapcar #'(lambda (a b) (list a b) ) data (reverse (cons (car data) (reverse (cdr data)))) )
      )

(defun in-range (number lower-bound upper-bound)  
  (and (>= number lower-bound)
       (<= number upper-bound)))


(defun sort-segments (data)
(loop :for pair :in (get-segments data)            
            :if (= (first (first pair)) (first (second pair))) :collect (sort pair #'< :key #'second) :into samex
            :else :collect (sort pair #'< :key #'first) :into samey
            :finally (return (list 
                              (sort samey #'< :key #'(lambda (x) (second (first x))))    
                              (sort samex #'< :key #'(lambda (x) (first  (first x))))                                                          
                              )
                             )))


;inside '(6 4)

;outside '(2 4)

;inside (2 9)

;outside '(2 12)

(defun segment-contained (coord filtered-range firstk)            
      (print (list "segment-contained"))
      (print filtered-range)
      (find (funcall firstk coord) filtered-range :key #'(lambda (x) (funcall firstk (first x))))
      )

(defun is-internal (coord filtered-range firstk)
      ;(print (list "calc odd" (funcall firstk coord) ))
      (oddp ( length (print (remove-if-not #'(lambda (x) (> (funcall firstk coord) x)) filtered-range :key #'(lambda (x) (funcall firstk  (first x)) ))))
      ))


(defun is-inside (coord segments firstk secondk)      
      (print (list firstk secondk))
      (let ((filtered-range (remove-if-not #'(lambda (x) 
                             (in-range  (funcall secondk coord) (funcall secondk (first x)) (funcall secondk (second x)))) 
                             (funcall secondk segments))))
            ;(print filtered-range)            
             (or 
             (segment-contained coord filtered-range firstk)
             (is-internal coord filtered-range firstk))
             
            )
      )

(defun test-xy (coord segments)
      (print (list "testing coord" coord))
      (and
       (print (is-inside coord segments #'first  #'second))
       (print (is-inside coord segments #'second #'first ))
       ))



(defun get-candidates (data)
      (sort (mapcar #'(lambda (x) (list (calculate-area x) x)) (generate-pairs data)) #'> :key #'first )
      )


(defun generate-coords (corners)      
      (loop 
       :with cornersx := (sort (mapcar #'first corners) #'< )
       :with cornersy := (sort (mapcar #'second corners) #'< )            
       :for x :from (first cornersx) :upto (second cornersx)
       :append ( loop
                :for y :from (first cornersy) :upto (second cornersy)
                :collect (list x y)))) 

(defun is-valid (corners segments) 
      (print (list "testing candidate" corners))
      (every #'(lambda (c) (test-xy c segments)) (generate-coords corners)))

(defun problem2 (path)
      (let* (
             (data (read-input path))
             (segments (sort-segments data))
             )
                        
            (print segments)
            ;(find-if #'(lambda (candidate) (is-valid candidate segments)) (get-candidates data) :key #'second)
            
      ; (list
              (test-xy '(7 4) segments)
      ;        (test-xy '(3 4) segments)
      ;        (test-xy '(12 4) segments)
      ;        (test-xy '(4 2) segments)
      ;        (test-xy '(9 2) segments)
      ; )
      
      )
)



(problem2 "2025/day9/test_input")
(problem2 "2025/day9/input")