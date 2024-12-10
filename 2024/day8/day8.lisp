
(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (str-to-list riv) )))



(defun load-map (file)
(list-to-2d-array (read-input file)))


(defun locate-antenas (input-map)
  (loop 
    :with rows := (array-dimension input-map 0)
    :with antenas := (make-hash-table)
    :for r :from 0 :below rows
    :do (loop
      :with cols := (array-dimension input-map 1)
      :for c :from 0 :below cols
      :for s := (aref input-map r c)
      :when (not (eq s #\.))
      :do (push (list r c) (gethash s antenas nil)))       
    :finally (return antenas)  
   )
  )

(defun resonate-first (a d dimensions)
  (let ((r (mapcar #'+ a d)))
    (when (validate-coord r dimensions)
      (list r)
    )))

(defun resonate-second (a d dimensions)
  (loop
   :for x := a :then (mapcar #'+ x d)
   :while (validate-coord x dimensions)
   :collect x
   )
  )


(defun get-antinodes (a b resonator)
  (let* (
         (d  (mapcar #'- a b))
         (n1 (funcall resonator a d))
         (n2 (funcall resonator b (mapcar #'- d)))
         )
    (append n1 n2)))

(defun get-all-antinodes (antenas resonator)
  (loop :for v :being :the hash-value :in antenas
      :append (mapcon (lambda (x) (mapcan (lambda(y) (get-antinodes (car x) y resonator)) (cdr x))) v)))

(defun validate-coord (coord dimensions)
  (every #'identity (mapcar (lambda (x y) (and (>= x 0) (< x y))) coord dimensions)))
  

(defun get-resonator (n dimensions)
  (cond   
    ((= n 1) (lambda (a d) (resonate-first a d dimensions)))
    ((= n 2) (lambda (a d) (resonate-second a d dimensions)))   
   )   
  )


(defun solve (file n)
  (let* (
         (input-map (load-map file))
         (antenas  (locate-antenas input-map))
         (resonator (get-resonator n (array-dimensions input-map)))
         )
    (length (remove-duplicates (get-all-antinodes antenas resonator) :test #'equalp ))      
    ))


(solve "2024/day8/test_input" 1)
(solve "2024/day8/input" 1)

(solve "2024/day8/test_input" 2)
(solve "2024/day8/input" 2)