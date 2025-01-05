(ql:quickload "Alexandria")

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun load-map (file)
  (with-open-file (in file)
  
     (list-to-2d-array (loop          
                  :for riv = (read-line in nil)                   
                  :while riv
                  :collect (str-to-list riv) ))))

(defmacro aref-2d (array indices)
  `(aref ,array (first ,indices) (second ,indices)))

(defun validate-coord (coord mapa)
  (and 
   (every #'identity (mapcar (lambda (x y) (and (>= x 0) (< x y))) coord (array-dimensions mapa)))
   (not (eq (aref-2d mapa coord) #\#))
   ))

(defun get-neighbours (c mapa)
  (remove-if-not (lambda (n) (validate-coord n mapa)) (mapcar (lambda (d) (mapcar #'+ c d)) '((0 1) (-1 0) (0 -1) (1 0) ))))

(defun validate-wall-coord (coord mapa)
  (and 
   (every #'identity (mapcar (lambda (x y) (and (>= x 0) (< x y))) coord (array-dimensions mapa)))
   (eq (aref-2d mapa coord) #\#)
   ))

(defun get-neighbour-walls (c mapa)
  (remove-if-not (lambda (n) (validate-wall-coord n mapa)) (mapcar (lambda (d) (mapcar #'+ c d)) '((0 1) (-1 0) (0 -1) (1 0) ))))


(defun build-graph (mapa)
  (loop
    :with graph := (make-hash-table :test 'equalp)
    :with rows := (array-dimension mapa 0)
    :with cols := (array-dimension mapa 1)
    :with start := nil
    :with end := nil
    :for row :from 0 :below rows
    :do (loop
      :for col :from 0 :below cols
      :for coord := (list row col)
      :for tile := (aref-2d mapa coord)
      :for n := (get-neighbours coord mapa)
      :when (not (eq (aref-2d mapa coord) #\#))
      :do 
      (when n (setf (gethash coord graph nil) n))
      (cond 
       ((eq (aref-2d mapa coord) #\S) (setq start coord))
       ((eq (aref-2d mapa coord) #\E) (setq end coord))))
   :finally (return (list graph start end))))
    


(defun get-leaves (visited-table)
  (loop :for key :being :the hash-keys :of visited-table
          :using (hash-value value)
          :when (not (third value))
          :collect key))

(defun first-or-self (element)  
  (if (listp element)
      (first element)
      element))

(defun find-exit-started (graph end visited)
 (loop
     :for leaves :=  (sort (get-leaves visited) #'< :key (lambda (node) (first (gethash node visited))) )
     :for proc-node := (first leaves)
     :for (proc-distance proc-stack) := (gethash proc-node visited)
     :while leaves
     :do (list proc-node proc-distance proc-stack )
     :do
      ;(format t "processing ~a, distance ~a, neighbours ~a~%" proc-node proc-distance (gethash proc-node graph) )
      (setf (gethash proc-node visited) (list proc-distance proc-stack t (alexandria:copy-hash-table visited)))
      (loop 
        :for n-node :in (gethash proc-node graph)
        :for (v-distance v-list v-processed) := (gethash n-node visited nil)
        :for candidate-distance := (1+ proc-distance)
        :when (not v-processed) 
        :do
          (cond
            ((not v-distance) (setf (gethash n-node visited nil) (list candidate-distance (cons proc-node proc-stack) nil nil))) 
            ((<  candidate-distance v-distance ) (setf (gethash n-node visited nil) (list candidate-distance (cons proc-node proc-stack) nil nil)))
            ;((=  candidate-distance v-distance ) (setf (gethash n-node visited nil) (list candidate-distance (union v-list (adjoin  proc-node proc-stack :test #'equalp) :test #'equalp) nil nil)))
          )
      )
     :finally (return (gethash end visited nil))
    ))

(defun find-exit (graph start end)
  (let ((visited (make-hash-table :test #'equalp)))
    (setf (gethash start visited nil) (list 0 nil nil nil))
    (find-exit-started graph end visited)
    ))

(defun modify-map (mapa wall-coord next-coord)
  (let ((mod-map (alexandria:copy-array mapa)))
    (setf (aref-2d mod-map wall-coord) #\.)
    (setf (aref-2d mod-map next-coord) #\#)
    mod-map
    ))

(defun zero-if-nil (element) (if element element 0))

(defun array-to-list (array)
  (loop :for i :below (array-dimension array 0)
        :collect (loop :for j :below (array-dimension array 1)
                       :collect (aref array i j))))

(defun print-map (mapa)
  (format t "~%~{~{~a~}~%~}"  (array-to-list mapa))
  mapa
  )

(defun alt-paths (coord next-coord mapa visited end)
(loop
           :for wall-coord :in (get-neighbour-walls coord mapa)
           :for (distance path) := (find-exit-started
                              (first (build-graph  (modify-map mapa wall-coord next-coord)))
                              end
                              (fourth (gethash coord visited)))
           :collect (list coord wall-coord distance path)
    ))

(defun alt-paths2 (coord next-coord mapa start end)
(loop
           :for wall-coord :in (get-neighbour-walls coord mapa)
           :for (distance path) := (find-exit
                              (first (build-graph  (modify-map mapa wall-coord next-coord)))
                           start   
                           end
                              )
           :collect (list coord wall-coord distance path)
    ))

(defun solve-first (file)
 (let ((mapa (load-map file)))
  (destructuring-bind (f-graph start end) (build-graph mapa)
    (destructuring-bind (f-length path success visited) (find-exit f-graph start end) 
         ;(remove-if-not (lambda (x) (and (third x) (> (- f-length (third x)) 0)))
            (loop
              :for next-coord := end :then coord
              :for coord :in path
              :do (list success)
              :append (alt-paths2 coord next-coord mapa start end))
          ;:key (lambda (x) (third x)) 
          ;)
        ))))

(untrace alt-paths)
(untrace find-exit-started)

(loop 
 :with catbag := (make-hash-table)
 :for d :in (mapcar (lambda (x) (and (third x) (- 84 (third x)))) (solve-first "2024/day20/test_input0"))
 :do (incf (gethash d catbag 0))
 :finally (maphash (lambda (k v) (format t "~a cheats save ~aps~%" v k)) catbag))


(solve-first "2024/day20/test_input0")
;(solve-first "2024/day2o/test_input0" '(6 6) 21)
 

;(build-graph (parse-input "2024/day18/test_input0"))

