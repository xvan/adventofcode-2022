(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun parse-input (file)
  (with-open-file (in file)
  
     (list-to-2d-array (loop          
                  :for riv = (read-line in nil)                   
                  :while riv
                  :collect (str-to-list riv) ))
  
    ))



(defun validate-coord (coord mapa)
  (and 
   (every #'identity (mapcar (lambda (x y) (and (>= x 0) (< x y))) coord (array-dimensions mapa)))
   (not (eq (aref-2d mapa coord) #\#))
   )
   
  )

(defun get-vertical-neighbours (c mapa)
  (remove-if-not (lambda (n) (validate-coord n mapa)) (mapcar (lambda (d) (mapcar #'+ c d)) '((-1 0) (1 0) ))))

(defun get-horizontal-neighbours (c mapa)
  (remove-if-not (lambda (n) (validate-coord n mapa)) (mapcar (lambda (d) (mapcar #'+ c d)) '((0 1) (0 -1)))))

(defmacro aref-2d (array indices)
  `(aref ,array (first ,indices) (second ,indices)))


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
      :for vn := (get-vertical-neighbours coord mapa)
      :for hn := (get-horizontal-neighbours coord mapa)
      :when (not (eq (aref mapa row col) #\#))
      :do 
      (when vn (setf (gethash (list coord 'v) graph nil) (mapcar (lambda (c) (list (list c 'v) 1)) vn)))
      (when hn (setf (gethash (list coord 'h) graph nil) (mapcar (lambda (c) (list (list c 'h) 1)) hn)))
      (when t ;(and vn hn)
            (push (list (list coord 'v) 1000) (gethash (list coord 'h) graph))
            (push (list (list coord 'h) 1000) (gethash (list coord 'v) graph))
            )
      (when (eq tile #\S) (push (list (list coord 'h) 0) (gethash 'S graph nil) ))
      (when (eq tile #\E) 
            (push (list 'E 0) (gethash (list coord 'h) graph))
            (push (list 'E 0) (gethash (list coord 'v) graph))
            )
      )
    :finally (return graph)))


(build-graph (parse-input "2024/day16/test_input0"))


(defun get-leaves (visited-table)
  (loop :for key :being :the hash-keys :of visited-table
          :using (hash-value value)
          :when (not (third value))
          :collect key))

(defun find-shortest-paths (graph)
  (let (
        (visited (make-hash-table :test #'equalp))        
        )
    (setf (gethash 'S visited nil) (list 0 nil nil))    

    (loop
     :for leaves := (sort (get-leaves visited) #'< :key (lambda (node) (first (gethash node visited))))
     :for proc-node := (first leaves)
     :for (proc-distance proc-stack) := (gethash proc-node visited)
     :while leaves
     :do
      (setf (gethash proc-node visited) (list proc-distance proc-stack t))
      (loop 
        :for (node distance) :in (gethash proc-node graph)
        :for (v-distance v-list v-processed) := (gethash node visited nil)
        :for candidate-distance := (+ proc-distance distance)
        ;:for debug_data := (format t "node:~a parent:~a processed:~a vdistance:~a cdistance:~a eval:~a~%" node proc-node v-processed v-distance candidate-distance (and (not v-processed) (not (and v-distance (<  candidate-distance v-distance )))))
        :when (or (not v-processed) (not v-distance) (>  v-distance candidate-distance  ))
        :do 
          (setf (gethash node visited nil) (list candidate-distance (cons (list proc-node distance) proc-stack) nil))
          ;(print node)
          ;(print (list node (gethash node visited nil)))
          ;(format t "node:~a cost:~a parent:~a~%" node (car (gethash node visited nil)) (caadr (gethash node visited nil)))
      )
     :finally (return visited)
    )))

(find-shortest-paths (build-graph (parse-input "2024/day16/pain2")))

(defvar *pain_visit*
 (find-shortest-paths (build-graph (parse-input "2024/day16/pain")))
)

(defvar *pain_visit2*
 (find-shortest-paths (build-graph (parse-input "2024/day16/pain2")))
)
(defun solve-first (file) 
 (first (gethash 'E (find-shortest-paths (build-graph (parse-input file)))))
 )

 (gethash '((2 5) V) *pain_visit*)

;(remove-if-not (lambda (x) (< (third x) current-cost))  )

;(mapcar (lambda (x) (nconc neighbours (gethash (first x) visited))) 

(defun extended-distance (n)
  (+ (second (first n)) (first (second n)))
  )

(defun backtrack-paths (graph visited node)
  (let* ((current-cost (first (gethash node visited)) )
         (neighbours (gethash node graph))
         (extended-neighbours (mapcar (lambda (x) (list x (gethash (first x) visited))) neighbours))
         (candidate-neighbours (remove-if-not (lambda (x) (< (first (second x)) current-cost)) extended-neighbours))
         (next-min (if candidate-neighbours (apply #'min (mapcar #'extended-distance candidate-neighbours)) -1))
         (chosen (remove-if-not (lambda (x) (= (extended-distance x) next-min)) candidate-neighbours))
         
      )
    ; (format t "current cost: ~a~%" current-cost)
    ; (format t "neighbours: ~a~%" neighbours)
    ; (format t "extended-neighbours: ~a~%" (mapcar (lambda (x) (list (caar x) (caadr x))) extended-neighbours))
    ; (format t "candidate-neighbours: ~a~%"candidate-neighbours)
    ; (format t "chosen: ~a~%" chosen)
    (loop :for chosen-node in chosen
       :append (mapcar (lambda (x) (cons node x)) (backtrack-paths graph visited (caar chosen-node))) :into all-paths
       :finally (return  (if all-paths all-paths (list (list node)))))
  ))

(defun array-to-list (array)
  (loop :for i :below (array-dimension array 0)
        :collect (loop :for j :below (array-dimension array 1)
                       :collect (aref array i j))))

(defun savetxt (filename mapa)
    (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "~{~{~a~}~%~}" (array-to-list mapa))    
  ))



(defun solve-second (file) 
(let* ((mapa (parse-input file))
       (graph (build-graph mapa))
       (shortest-paths  (find-shortest-paths graph))
       (winning-path (second (gethash 'E shortest-paths)))
       (end-node (caar (cdr winning-path)))
       (paths (backtrack-paths graph shortest-paths end-node))
       (reduced-paths (mapcar (lambda (path) (reverse (reduce (lambda (x y) (adjoin y x :test #'equalp)) path :key #'first :initial-value nil ))) paths))
       (joint-paths (reduce (lambda (x y) (union x y :test #'equalp)) reduced-paths))
      )
    ; (format t "path: ~a~%" paths)
    ; (format t "redu: ~a~%" reduced-paths)
    ; (format t "join: ~a~%" joint-paths)
    ;(format t "pathlen ~a~%" (first (gethash 'E shortest-paths)))
    ;(loop :for coord :in (get-pain) :do (setf (aref-2d mapa coord) #\X))
    (loop :for coord :in joint-paths :do (setf (aref-2d mapa coord) #\O))
    (savetxt "2024/day16/output-p2" mapa)
    (1+ (length joint-paths))
    ))

(untrace backtrack-paths)
;(untrace backtrack-paths)
;(solve-second "2024/day16/test_input0")
;(solve-second "2024/day16/test_input1")
;(solve-second "2024/day16/input-mod")
(solve-second "2024/day16/input")
(solve-second "2024/day16/pain")
;(solve-second "2024/day16/input-short")
;(solve-second "2024/day16/input-short")
; (solve-first "2024/day16/test_input0")
; (solve-first "2024/day16/test_input1")
;(solve-first "2024/day16/input")




;(find-exit (build-graph (parse-input "2024/day16/test_input0")))
;(find-exit (build-graph (parse-input "2024/day16/test_input1")))
;(find-exit (build-graph (parse-input "2024/day16/input")))

