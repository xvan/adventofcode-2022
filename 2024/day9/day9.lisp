(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun read-diskmap (file)
  (with-open-file (in file)
  (mapcar (lambda (c) (- (char-code c) (char-code #\0))) 
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :append (str-to-list riv) ))))


(defun explode-blocks (diskmap)  
  (loop :for (a b) :on diskmap :by #'cddr
        :for i :from 0
        :append (append 
                    (loop :for j :from 0 :below a :collect i)
                    (loop :for j :from 0 :below (or b 0) :collect nil)
                ) :into blocks-list
        :finally (return (make-array (length blocks-list) :initial-contents blocks-list))
  )
  )


(defun next-empty (seq idx)
  (loop :for ff :from idx :below (length seq) :unless (aref seq ff) :return ff ))

(defun prev-filled (seq idx)
  (loop :for rew :downfrom idx :to 0 :when (aref seq rew) :return rew ))

(defun swap-array-values (array index1 index2)
  (let ((temp (aref array index1)))
    (setf (aref array index1) (aref array index2))
    (setf (aref array index2) temp)))

(defun defrag-blocks (blocks)
  (loop        
        :for ff := (next-empty blocks 0) :then (next-empty blocks ff)
        :for rew := (prev-filled blocks (1- (length blocks))) :then (prev-filled blocks rew)
        :when (<= rew ff) :return blocks
        :do (swap-array-values blocks ff rew)
      )
  )

(defun next-file (blocks idx)
  (loop
    :with file-end := (loop :for cur :downfrom idx :to 0 :when (aref blocks cur) :return cur)
    :for cur :from file-end :downto 0
    :when (not (eq (aref blocks cur) (aref blocks file-end)))
      :return (list (1+ cur) (1+ file-end))
    :finally (return (list 0 (1+ file-end)))
   )
)

(defun next-empty-range (blocks idx n limit)
  (loop
   :with empty-start := (loop :for cur :from idx :to limit :unless (aref blocks cur) :return cur)
   :for cur :from (print empty-start) :below limit
   :for cnt :from 1 :to n
   :when (aref blocks cur)
    :return (list empty-start (1- n))
   :finally (return (list empty-start n))
  ))
 

(next-file (explode-blocks  (read-diskmap"2024/day9/test_input")) )

(defun calc-checksum (blocks)
  (loop 
           :for id :across blocks
           :for b :from 0           
           :sum (* (or id 0) b))      
  )

(defrag-blocks (explode-blocks  (read-diskmap"2024/day9/test_input")))

(defun solve (file n)
  (let ((blocks (explode-blocks (read-diskmap file))))
    (cond 
     ((= n 1) (calc-checksum (defrag-blocks blocks)))
     ((= n 2) (next-empty-range (print blocks) 3 3 (length blocks));(next-file (print blocks) 4))
       ))))


(solve "2024/day9/test_input" 1)
(solve "2024/day9/input" 1)

(solve "2024/day9/test_input" 2)
(solve "2024/day9/input" 2)