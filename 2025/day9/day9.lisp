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
      (identity (list "segment-contained"))
      (identity filtered-range)
      (find (funcall firstk coord) filtered-range :key #'(lambda (x) (funcall firstk (first x))))
      )

(defun is-internal (coord filtered-range firstk secondk)
      ;(print (list "calc odd" (funcall firstk coord) ))
      (let* (
             (crossed-segments (remove-if-not #'(lambda (x) (> (funcall firstk coord) x)) filtered-range :key #'(lambda (x) (funcall firstk  (first x)) )))
             (crossed-corners  (remove-if-not #'(lambda (x) (some #'(lambda (y) (= (funcall secondk coord) (funcall secondk y))) x)) crossed-segments))
             )
            
            (identity (list "firstk coord" (funcall firstk coord)))
            (identity (list "crossed-segments" crossed-segments))
            (identity (list "crossed-corners" crossed-corners))
            (oddp (- (length crossed-segments)(floor (length crossed-corners) 2)))      
            )      
      )




(defun is-inside (coord segments firstk secondk)      
      (identity (list firstk secondk))
      (let ((filtered-range (remove-if-not #'(lambda (x) 
                             (in-range  (funcall secondk coord) (funcall secondk (first x)) (funcall secondk (second x)))) 
                             (funcall secondk segments))))
            ;(print filtered-range)            
             (or 
             (segment-contained coord filtered-range firstk)
             (is-internal coord filtered-range firstk secondk))
             
            )
      )

(defun test-xy (coord segments cache)
      (identity (list "testing coord" coord))
      (multiple-value-bind (value key-exists-p) (gethash coord cache)
      (if key-exists-p
          value
          (setf (gethash coord cache) 
            (and
            (identity (is-inside coord segments #'first  #'second))
            (identity (is-inside coord segments #'second #'first ))
            )))))



(defun get-candidates (data)
      (sort (mapcar #'(lambda (x) (list (calculate-area x) x)) (generate-pairs data)) #'> :key #'first )
      )


(defun generate-rectangle-coords (corners coord-tics)
      coord-tics      
      (let* (
             (cornersx (sort (mapcar #'first corners) #'< ))
             (cornersy (sort (mapcar #'second corners) #'< ))     
             (coordsx (remove-if-not #'(lambda (x) (in-range x (first cornersx) (second cornersx))) (first coord-tics)))
             (coordsy (remove-if-not #'(lambda (y) (in-range y (first cornersy) (second cornersy))) (second coord-tics)))              
      )
      (generate-coord-mesh (list coordsx coordsy)))       
      )


(defun is-valid (corners cache coord-tics) 
      (identity (list "testing candidate" corners))
      (every #'(lambda (c) (gethash c cache)) (generate-rectangle-coords corners coord-tics)))

(defun generate-coord-tics (data)
      (loop
       :with hashx := (make-hash-table )
       :with hashy := (make-hash-table )
       :for coord :in data
       :do (setf (gethash (first coord) hashx) t)       
       :do (setf (gethash (second coord) hashy) t)
       :finally (return (list                    
                  (sort (loop for key being the hash-keys of hashx collect key) #'< )
                  (sort (loop for key being the hash-keys of hashy collect key) #'< )
                   ))
      )
)

(defun generate-coord-mesh (coord-tics)
      (loop 
       :with xcoords := (first coord-tics)
       :with ycoords := (second coord-tics)
       :for x :in xcoords
       :append (loop :for y :in  ycoords :collect (list x y))
      )
)

(defun generate-filled-cache (reduced-coords segments)
      (let ((cache (make-hash-table :test 'equal)))
      ( loop 
             :for c :in reduced-coords  
             :do ( setf (gethash c cache) (test-xy c segments cache)) 
             :finally (return cache)             
             )
      ))


(defun save-map-as-pgm (filename cache square coord-tics)
  square
  coord-tics
  (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((width (length (first coord-tics)))
          (height (length (second coord-tics)))
                  )
      ;; Write PBM header
      (format out "P2~%")
      (format out "~d ~d~%" width height)
      (format out "3~%")
      ;; Write PBM data
      (loop 
            :for yc :in (second coord-tics)
            :for y :from 0
            :do (loop 
                  :with rc := (generate-rectangle-coords square coord-tics)
                  :for xc :in (first coord-tics)
                  :for x :from 0
                      :do (format out "~a " 
                      (cond
                       ( (not (gethash (list xc yc) cache)) 3)
                       ( (find (list xc yc) rc :test #'equalp) 2)
                       ( t 0)))
                      
            (format out "~%"))))))

(defun problem2 (path)
      (let* (
             (data (read-input path))
             (coord-tics (generate-coord-tics data))             
             (reduced-coords (generate-coord-mesh coord-tics))
             ;(coord-validator (lambda (c) (and (gethash (first c) (first coord-hash)) (gethash (second c) (second coord-hash)))))
             (segments (sort-segments data))
             (cache (generate-filled-cache  reduced-coords segments))
             )             
            
            ;(loop :for c :in reduced-coords :do (print (list c (gethash c cache))))
            (print "termine")
            ;(length reduced-coords)
            ;(identity segments)
            (save-map-as-pgm "2025/day9/test.pgm" cache 
                             (second (find-if #'(lambda (candidate) (is-valid candidate cache coord-tics)) (get-candidates data) :key #'second)) 
                             coord-tics)
            
            

      ; (list
      ;        (remove-if-not coord-validator '((7 4) (11 1) (9 1)))
      ;        (test-xy '(7 4) segments)
      ;        (test-xy '(3 4) segments)
      ;        (test-xy '(12 4) segments)
      ;        (test-xy '(4 2) segments)
      ;        (test-xy '(9 2) segments)
      ; )
      
      )
)
;(problem2 "2025/day9/test_input")
(problem2 "2025/day9/input")
