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

(defun find-exit (graph)
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
        :for (node distance) :in (gethash (first leaves) graph)
        :for (v-distance v-list v-processed) := (gethash node visited nil)
        :for candidate-distance := (+ proc-distance distance)
        :when (and (not v-processed) (not (and v-distance (<  candidate-distance v-distance ))))
        :do 
          (setf (gethash node visited nil) (list candidate-distance (cons (list proc-node distance) proc-stack) nil))
          ;(print node)
          ;(print (gethash node visited nil))
      )
     :finally (return (gethash 'E visited))
    )))



(find-exit (build-graph (parse-input "2024/day16/test_input0")))
(find-exit (build-graph (parse-input "2024/day16/test_input1")))
(find-exit (build-graph (parse-input "2024/day16/input")))

