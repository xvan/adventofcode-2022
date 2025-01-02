(ql:quickload "cl-ppcre")

(defun read-input (file)
  (with-open-file (in file)
   (loop
     :with patterns := (cl-ppcre:split "(,\\s)" (read-line in nil) )
     :initially  (read-line in nil)
     :for riv := (read-line in nil)
     :while riv
     :collect riv :into designs
     :finally (return (list patterns designs))
     )
  )
  )

(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun generate-hashtree (patterns)
  (loop 
   :with hashtree := (make-hash-table)
   :for pattern :in patterns
   :do (loop
      :for c :in (str-to-list pattern) 
      :for node-hash := hashtree :then next-node-hash
      :for (w next-node-hash) := (gethash c node-hash nil)
      :when (not next-node-hash)
      :do (setf next-node-hash (second (setf (gethash c node-hash) (list nil (make-hash-table)))))
      :finally (setf (gethash c node-hash) (list pattern next-node-hash))
      )
   :finally (return hashtree)
   )
   )
  
(defmacro cons-not-nil (A B)
  `(if ,A (cons ,A ,B) ,B)
  )

(defun get-match (pattern idx hashtree)
  (when (< idx (length pattern))
   ( destructuring-bind (match next-hashtree) (gethash (char pattern idx) hashtree '(nil nil)) 
    (cons-not-nil match (when next-hashtree (get-match pattern (1+ idx) next-hashtree)))))
  )

(defun solve-first (file)
(destructuring-bind (patterns designs) (read-input file)
  (let ((hashtree (generate-hashtree patterns)))
    ( loop 
     :for design :in designs
     ;:do (format t "TARGET ~a~%" design)
     :count
     (loop
      :for matches := '(0) 
      :then (remove-if (lambda(c) (find c processed)) 
             (remove-duplicates 
                (loop :for match :in matches :append 
                        (mapcar (lambda (x) (+ match (length x))) 
                            (get-match design match hashtree) ) ) ))
      :for processed := (reduce (lambda (a b) (adjoin b a)) matches :initial-value processed)
      ;:do (format t "MATCH-COLLECTION ~a~%" matches)
      :while matches
      :when (some (lambda (x) (= (length design) x)) matches)
      :return t
      ))))
  )



(defun recurse-match (pattern idx hashtree pathsgraph)
  (loop
   :for match :in (mapcar (lambda (x) (+ idx (length x))) (get-match pattern idx hashtree))
   :for visited := (gethash match pathsgraph nil)
   :do (progn 
      (push idx (gethash match pathsgraph nil))
      (when (not visited) (recurse-match pattern match hashtree pathsgraph))
   )
  )
)

(defun recurse-cnt (tkn pathsgraph)
  ;(reduce #'+ (mapcar (lambda (x) (if (= 0 x) 1 (recurse-cnt x pathsgraph))) (gethash tkn pathsgraph)))
  (loop
   :with visited := (make-hash-table)
   :for current := tkn :then (car queue)
   :for cnt := 1 :then (gethash current visited 0) 
   :for new-nodes :=  (gethash current pathsgraph nil)
   :for queue :=  (sort (reduce (lambda (a b) (adjoin b a)) new-nodes :initial-value (cdr queue)) #'>)
   :do (mapc (lambda (x) (incf (gethash x visited 0) cnt) ) new-nodes)
   :until (not queue)
   :finally (return (gethash 0 visited 0))
   )
  )

(defun solve-second (file)
(destructuring-bind (patterns designs) (read-input file)
    ( loop 
       :with hashtree := (generate-hashtree patterns)
       :for design :in designs
       :for x :from 0
       :for pathsgraph := (make-hash-table)
       ;:do (format t "~a: ~a~%" x design)
       :do (recurse-match design 0 hashtree pathsgraph)
       ;:do (format t "counting...~%")
       :sum (recurse-cnt (length design) pathsgraph)
  )))



;(solve-first "2024/day19/test_input0")
;(solve-first "2024/day19/input")
(solve-second "2024/day19/test_input0")
(solve-second "2024/day19/input")
