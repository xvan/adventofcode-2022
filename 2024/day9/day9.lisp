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
    (setf (aref array index2) temp)
    array
    ))

(defun defrag-blocks (blocks)
  (loop        
        :for ff := (next-empty blocks 0) :then (next-empty blocks ff)
        :for rew := (prev-filled blocks (1- (length blocks))) :then (prev-filled blocks rew)
        :when (<= rew ff) :return blocks
        :do (swap-array-values blocks ff rew)
      )
  )

; (defun next-file (blocks idx)
;   (loop
;     :with file-end := (loop :for cur :downfrom idx :to 0 :when (aref blocks cur) :return cur)
;     :for cur :from file-end :downto 0
;     :when (not (eq (aref blocks cur) (aref blocks file-end)))
;       :return (list (1+ cur) (1+ file-end))
;     :finally (return (list 0 (1+ file-end)))
;    )
; )

(defun next-file (blocks idx)
  (progn
  (print (format nil "next-file called with idx: ~A" idx))
  (loop
    :with file-end := (print (loop :for cur :downfrom idx :to 0 :when (aref blocks cur) :return cur))
    :for cur :from file-end :downto 0
    :when (not (eq (aref blocks cur) (aref blocks file-end)))
      :do (print (format nil "Returning: (~A ~A)" (1+ cur) (1+ file-end)))
      :return (list (1+ cur) (1+ file-end))    
   ))
)



(defun next-empty-range (blocks idx limit)
  (loop
   :with empty-start := (loop :for cur :from idx :to limit :unless (aref blocks cur) :return cur)
   :for cur :from  empty-start :below limit
   :for end :from empty-start
   :until (aref blocks cur)    
   :finally (return (list empty-start end))
  ))

(defun find-empty-range (blocks limit n)
  (loop
   :for (ecur eend) := (next-empty-range blocks 0 limit) :then (next-empty-range blocks eend limit)
   :while (> n (- eend ecur))
   :finally (return (list ecur eend))
   ))
 


(next-empty-range #(nil nil 3 3 3 nil nil nil 3 3 3) 2 100)

(defun defrag-files (blocks)
  (loop
   :for (fcur fend) := (print (next-file blocks (1- (length blocks)))) :then (print (next-file blocks (1- fcur)))
   :for (ecur eend) := (print (find-empty-range blocks fcur   (- fend fcur))  )
   :until (= 0 (print fcur))
   :when (print (>= (- eend ecur)  (- fend fcur)))
   :do 
    ;;(print (list (list ecur eend) (list fcur fend)))
    ;;(print (loop :for i :from fcur :below fend :collect (aref blocks i)))
    (loop
      :for i :from fcur :below fend
      :for j :from ecur :below eend
      :do (swap-array-values blocks i j)
      :finally (print blocks)            
      )
   :finally (return blocks)
   )
  )

(defun calc-checksum (blocks)
  (loop 
           :for id :across blocks
           :for b :from 0           
           :sum (* (or id 0) b))      
  )

(defun solve (file n)
  (let ((blocks (explode-blocks (read-diskmap file))))
    (cond 
     ((= n 1) (calc-checksum (defrag-blocks blocks)))
     ((= n 2) (defrag-files (print blocks)));(next-file (print blocks) 4))
       )))


(solve "2024/day9/test_input" 1)
(solve "2024/day9/input" 1)

(solve "2024/day9/test_input" 2)
;;(solve "2024/day9/input" 2)