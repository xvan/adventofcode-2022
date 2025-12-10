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

(defun generate-coord-mesh2 (coord-tics)
      (loop 
       :with xcoords := (first coord-tics)
       :with ycoords := (second coord-tics)
       :for x :in xcoords
       :collect (loop :for y :in  ycoords :collect (list x y))
      )
)




(defun test-edge (line edge)
      (cond
       ( (= line (first (first edge))) 1)
       ( (= line (first (second edge))) -1)
       ( (and (> line (first (first edge))) (< line (first (second edge)))) 0)
       ( t nil)
       ))      
       

(defun get-intersections-and-type (line edges)      
      (remove-if-not #'identity (mapcar #'(lambda (c) (list (second (first c)) (test-edge line c))) edges) :key #'second)  
      )



(defun scanline (data)
      (let (
            (edges (first (sort-segments data)))
            ;(minx (apply #'min (mapcar #'first data)))
            (maxx (apply #'max (mapcar #'first data)))
            ;(miny (apply #'min (mapcar #'second data)))
            (maxy (apply #'max (mapcar #'second data)))
            )
            (print (get-intersections-and-type 1 edges))
            
            ( loop 
              :for x :from 0 :upto maxx
              :collect (loop
                 :with intersection-and-type := (get-intersections-and-type x edges)
                 :with pen := nil
                 :with last-type := 0                                  
                 :for y :from 0 :upto maxy
                 :collect (if (cond
                     ((not intersection-and-type) nil) 
                     ((< y (first (car intersection-and-type))) pen)
                     (t (let ((current-type (second (pop intersection-and-type))))
                             (cond
                                   ((not pen) (setf pen t) (setf last-type current-type))  
                                   ((= 0 current-type) (setf pen nil) (setf last-type current-type))
                                   ((= last-type current-type) (setf pen nil)) ; handles (1 1) (-1 -1)                                   
                                   (t nil); handles (1 1) (-1 -1)
                              ) t ;allways print at matching coord
                                  ))
                     ) #\# #\.)
                 )                                        
            )
            
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


(defun generate-scanline-cache (coord-tics edges)      
      (let ((cache (make-hash-table :test 'equalp)))      
            (loop :for x :in (first coord-tics)
                  :do (loop                  
                       :with intersection-and-type := (get-intersections-and-type x edges)
                       :with pen := nil
                       :with last-type := 0
                       :for y in (second coord-tics)
                       :when (cond
                              ((not intersection-and-type) nil) 
                              ((< y (first (car intersection-and-type))) pen)
                              (t (let ((current-type (second (pop intersection-and-type))))
                                       (cond
                                        ((not pen) (setf pen t) (setf last-type current-type))  
                                        ((= 0 current-type) (setf pen nil) (setf last-type current-type))
                                        ((= last-type current-type) (setf pen nil)) ; handles (1 1) (-1 -1)                                   
                                        (t nil); handles (1 1) (-1 -1)
                                        ) t ;allways print at matching coord
                                       )))
                        :do (setf (gethash (list x y) cache) t)
                       )
                  :finally (return cache))))

(defun problem2 (path)
      (let* (
             (data (read-input path))
             (coord-tics (generate-coord-tics data))                          
             (segments (first (sort-segments data)))
             (cache (generate-scanline-cache coord-tics segments))
             )             
            (print "termine cache")            
            (find-if #'(lambda (candidate) (is-valid candidate cache coord-tics)) (get-candidates data) :key #'second)
            
            
            ; (save-map-as-pgm "2025/day9/test.pgm" cache 
            ;                  (second (find-if #'(lambda (candidate) (is-valid candidate cache coord-tics)) (get-candidates data) :key #'second)) 
            ;                  coord-tics)
            
            

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


(defparameter **body-man** '(
                       (1 4)
                       (1 6)
                       (3 6)
                       (3 10)
                       (7 10)
                       (7 8)
                       (10 8)
                       (10 6)
                       (5 6)
                       (5 3)
                       (10 3)
                       (10 1)
                       (5 1)
                       (5 0)
                       (3 0)
                       (3 4)
                       ))

(defun generate-body ()
      (let (
      ;      (minx (apply #'min (mapcar #'first **body-man**)))
            ;(maxx (apply #'max (mapcar #'first **body-man**)))
      ;      (miny (apply #'min (mapcar #'second **body-man**)))
            ;(maxy (apply #'max (mapcar #'second **body-man**)))
            (maxx 12) (maxy 12)
            (filename "2025/day9/body.pgm")
            )


      

      (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out "P2~%")
      (format out "~d ~d~%" maxx maxy)
      (format out "3~%")

      (loop             
            :for x :from 0 :below maxx
            :do (loop                   
                  :for y :from 0 :below maxy
                      :do (format out "~a " 
                      (cond                       
                       ( (find (list x y)  **body-man** :test #'equalp) (and (print (list x y)) 1))
                       ( t 3))))
                   (format out "~%")
                   ))))


      
(format nil "~{~{~a~}~%~}" (scanline **body-man**))


(mapcar #'(lambda (x) x) (sort-segments **body-man**) )


(segment-contained '(1 7) (second (sort-segments **body-man**)) #'first )

'(NIL 
  ((4 1) (6 1))
  ((4 0) (6 0))
  ((0 1) (4 -1) (6 -1) (10 1))
  ((0 0) (10 0))
  ((0 -1) (1 1) (3 1) (6 1) (10 0))
  ((1 0) (3 0) (6 0) (10 0))
  ((1 0) (3 0) (6 0) (8 1) (10 -1)) 
  ((1 0) (3 0) (6 0) (8 0))
  ((1 0) (3 0) (6 0) (8 0)) 
  ((1 -1) (3 -1) (6 -1) (8 -1)))
;(generate-body)