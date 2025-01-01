(ql:quickload "cl-ppcre")

(defun parse-input (file)
  (with-open-file (in file)
  
     (loop          
        :for riv = (read-line in nil)                   
        :while riv
        :collect (mapcar #'parse-integer (cl-ppcre:split "," riv)) )))



(defmacro aref-2d (array indices)
  `(aref ,array (first ,indices) (second ,indices)))


(defun load-map (coords dims steps)
  (loop
   :with mapa := (make-array dims :initial-element #\.)
   :for dummy :below steps
   :for coord :in coords
   :do (setf (aref-2d mapa coord) #\# )
   :finally (return mapa)
  )
  )

(defun validate-coord (coord mapa)
  (and 
   (every #'identity (mapcar (lambda (x y) (and (>= x 0) (< x y))) coord (array-dimensions mapa)))
   (not (eq (aref-2d mapa coord) #\#))
   )
  )

(defun get-neighbours (c mapa)
  (remove-if-not (lambda (n) (validate-coord n mapa)) (mapcar (lambda (d) (mapcar #'+ c d)) '((0 1) (-1 0) (0 -1) (1 0) ))))


(defun build-graph (mapa)
  (loop
    :with graph := (make-hash-table :test 'equalp)
    :with rows := (array-dimension mapa 0)
    :with cols := (array-dimension mapa 1)
    :for row :from 0 :below rows
    :do (loop
      :for col :from 0 :below cols
      :for coord := (list row col)
      :for tile := (aref-2d mapa coord)
      :for n := (get-neighbours coord mapa)
      :when (not (eq (aref mapa row col) #\#))
      :do 
      (when n (setf (gethash coord graph nil) n)))
    :finally (return graph)))


(defun get-leaves (visited-table)
  (loop :for key :being :the hash-keys :of visited-table
          :using (hash-value value)
          :when (not (third value))
          :collect key))

(defun first-or-self (element)  
  (if (listp element)
      (first element)
      element))

(defun find-exit (graph start end)
  (let (
        (visited (make-hash-table :test #'equalp))        
        )
    (list start end)
    (setf (gethash start visited nil) (list 0 nil nil))    

    (loop
     :for leaves :=  (sort (get-leaves visited) #'< :key (lambda (node) (first (gethash node visited))) )
     :for proc-node := (first leaves)
     :for (proc-distance proc-stack) := (gethash proc-node visited)
     :while leaves
     :do
      ;(format t "processing ~a, distance ~a, neighbours ~a~%" proc-node proc-distance (gethash proc-node graph) )
      (setf (gethash proc-node visited) (list proc-distance proc-stack t))
      (loop 
        :for n-node :in (gethash proc-node graph)
        :for (v-distance v-list v-processed) := (gethash n-node visited nil)
        :for candidate-distance := (1+ proc-distance)
        :when (not v-processed) 
        :do
          (cond
            ((not v-distance) (setf (gethash n-node visited nil) (list candidate-distance (adjoin proc-node proc-stack :test #'equalp) nil))) 
            ((<  candidate-distance v-distance ) (setf (gethash n-node visited nil) (list candidate-distance (adjoin proc-node proc-stack :test #'equalp) nil)))
            ((=  candidate-distance v-distance ) (setf (gethash n-node visited nil) (list candidate-distance (union v-list (adjoin  proc-node proc-stack :test #'equalp) :test #'equalp) nil)))
          )
      )
     :finally (return (gethash end visited nil))
    )))

(defun solve-first (file exit steps)
  (car (find-exit
    (build-graph (load-map (parse-input file) (mapcar #'1+ exit) steps) ) '(0 0) exit)))

(solve-first "2024/day18/test_input0" '(6 6) 21)
(solve-first "2024/day18/input" '(70 70) 1024)

(defun solve-second (file exit steps) 
  (loop 
    :with input := (parse-input file)
    :with accept-bound := steps
    :with reject-bound := (1- (length input))
    :with mapdims := (mapcar #'1+ exit)
    :while (> (- reject-bound accept-bound) 1)
    :for bissect := (floor (+ accept-bound reject-bound) 2)
    :do (if
      (find-exit (build-graph (load-map input mapdims bissect) ) '(0 0) exit)
      (setf accept-bound bissect)
      (setf reject-bound bissect)
      )
    :finally (return  (format nil "~{~a~^,~}" (nth accept-bound input)))
  )
)

(solve-second "2024/day18/test_input0" '(6 6) 0)

(time (solve-second "2024/day18/input" '(70 70) 0))






 

;(build-graph (parse-input "2024/day18/test_input0"))

