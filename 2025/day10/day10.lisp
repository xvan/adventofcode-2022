;(problem1 "2025/day10/input")      

(ql:quickload "lla")
(ql:quickload :array-operations)

(defun split-integers (riv)
  (loop :with start := 0 
        :for end := (search "," riv :start2 start)
        :collect (parse-integer (subseq riv start end))
        :while end
        :do (setf start (1+ end))                
  )
  )

(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun load-target (yow)
  (
   loop 
   :with tgt := 0
   :for c :in (reverse (str-to-list yow))
   :do (setf tgt (+ (ash tgt 1) (if (eq #\# c) 1 0)))
   :finally (return tgt)
   )
  )

(load-target ".##.")

(defun load-factors (button)
  (apply #'+ (mapcar #'(lambda (x) (ash 1 x)) button))
  )

(load-factors '(2 1))

(defun parse-line (riv)
  (let ((last-idx 0)
        (parsed-input nil))
    ;parse target
    (push (load-target (subseq riv (1+ (search "[" riv )) (setf  last-idx (search "]" riv )))) parsed-input)    
    ;parse parentheses
    (push    
       (identity (loop
     :for start := (search "(" riv :start2 (1+ last-idx))
     :while start
     :collect (split-integers (subseq riv (1+ start) (setf  last-idx (search ")" riv :start2 start))))
      )) parsed-input)
    (push
    (split-integers
    (subseq riv (1+ (search "{" riv :start2 (1+ last-idx))) (setf last-idx (search "}" riv :start2 last-idx))))
      parsed-input)
  
    (reverse parsed-input)
      )  
)

(defun read-input (file)
  (with-open-file (in file)
  (loop           :for riv = (read-line in nil)                  
                  :while riv                  
                  :collect (parse-line riv))))

(defun gospers-next (v)
  "Return the next larger integer with the same number of 1-bits.
   Returns NIL if no such number exists."
  (let* ((c (logand v (- v)))
         (r (+ v c)))
    (if (= r v)   ; overflow (no next)
        nil
        (logior r
                (truncate (ash (logxor r v) -2) c)))))


(defun generate-by-bitcount (n)
  "Return a list of all integers 0..(2^n - 1)
   ordered by increasing number of bits set."
  (let ((limit (ash 1 n))
        (result '()))
    (push 0 result)
    (loop for k from 1 to n do
      (let ((v (1- (ash 1 k))))   ; smallest number with k bits: 0...011..1
        (loop while (< v limit) do
          (push v result)
          (setf v (or (gospers-next v) limit)))))
    (nreverse result)))

(defun generate-by-kbitcount (n k)  
  (let ((limit (ash 1 n))        
      (v (1- (ash 1 k))) ; smallest number with k bits: 0...011..1
      )                  
      (loop while (< v limit)
        :collect v
        :do (setf v (or (gospers-next v) limit))))
  )

(defun bits-on (n)
  "Return list of indices of 1-bits (LSB = 0), using n & (n-1) trick."
  (let ((result '()))
    (loop while (> n 0) do
      (let* ((lsb (logand n (- n)))         ; isolate lowest set bit
             (idx (integer-length (1- lsb)))) ; log2(lsb)
        (push idx result)
        (setf n (logand n (1- n)))))        ; clear lowest 1-bit
    result))




(defun problem1 (path) 
  (reduce '+
   (loop 
   :for machine-manual :in (read-input path)
   :for lights := (first machine-manual)
   :for buttons :=  (identity (apply #'vector (mapcar #'load-factors (second machine-manual))))
   :collect (loop
           :with n := (length buttons)
           :for k :from 0 :upto n
           :for combinations := (generate-by-kbitcount n k)           
           :when (loop
              :for combination :in combinations
              :for idx := (bits-on combination)
              :for button-candidates := (mapcar #'(lambda (c) (aref buttons c)) idx)
              :for xorall := (apply #'logxor button-candidates)
              :do (identity (list lights xorall button-candidates ))
              :when (= 0 (logxor lights xorall))
              :return (identity t))
           :return k)))
  )



(problem1 "2025/day10/test_input")
(problem1 "2025/day10/input")


(defun button-array (buttons rows cols)
  (loop
   :with A := (make-array (list rows cols) :initial-element 0)
   :for button :in buttons
   :for c :from 0
   :do (mapc #'(lambda (r) (setf (aref A r c) 1)) button)
   :finally (return A)
   )
)



(defun array-to-list (array)
  (loop :for i :below (array-dimension array 0)
        :collect (loop :for j :below (array-dimension array 1)
                       :collect (aref array i j))))


(defun format-matrix (m)
  (format nil "~%~{~{~a~}~%~}"  (array-to-list m))    
  )

(defun get-basis (A)
    (print "getbasis que paso")  
    (print A)
    (print (lla:qr A))
    (loop 
     :with rows := (array-dimension A 0)
     :with cols := (array-dimension A 1) 
     :with covered := (make-array rows :initial-element nil)
     :for y :from 0 :below cols
     :if (loop 
          :with used := nil
          :for x :from 0 :below rows
          :when (and (= 1 (aref A x y)) (not (aref covered x)) )
          :do (setf (aref covered x) (setf used t))           
          :finally (return used))
      :collect y :into basis
      :else
      :collect y :into others
      :finally (return (list basis others))
  )
)

(defun copy-array (array &key (element-type (array-element-type array))
                             (fill-pointer t)
                             (adjustable t))
  "Return a fresh copy of ARRAY with the same dimensions and contents."
  (let ((dims (array-dimensions array)))
    (let ((new (make-array dims
                           :element-type element-type
                           :adjustable (and adjustable (adjustable-array-p array))
                           :fill-pointer (cond ((eq fill-pointer t)
                                                (and (array-has-fill-pointer-p array)
                                                     (fill-pointer array)))
                                               ((numberp fill-pointer) fill-pointer)
                                               (t nil)))))
      (dotimes (i (array-total-size array) new)
        (setf (row-major-aref new i)
              (row-major-aref array i))))))

(defun get-basis2 (Am)
    (let* (
           (A    (copy-array Am))
           (rows (array-dimension A 0))
           (cols (array-dimension A 0))
           (col-candidates (loop :for x :from 0 :below cols :collect x))                   
         )        
    (loop :for row :from 0 :below rows
     :for current-col := (loop for col :in col-candidates :when (not (= 0 (aref A row col))) :return col)  
     :do (setf col-candidates (remove current-col col-candidates))
      :do (print (list col-candidates current-col row ))
     :do (loop 
          :for minor-col :in col-candidates          
          :do ( loop       
                  :with scale := (/ (aref A row minor-col) (aref A row current-col))
                  :for minor-row :from 0 :below rows
                  :do (decf (aref A minor-row minor-col) 
                            (* scale (aref A minor-row current-col)))                  
                  )
               :finally (print (list rows cols col-candidates A))
                )
      :collect current-col   
      :while col-candidates  
      )
      
            )      
    )


(get-basis2 #2A((1 0 1 1 0) (0 0 0 1 1) (1 1 0 1 1) (1 1 0 0 1) (1 0 1 0 1)) )
  

(defun generate-basis (m basis)
  (print "m y basis")
  (print m)
  (print basis)
  (let* ((rows (array-dimension m 0))
         (cols (length basis))
         (contents
           (loop :for x :from 0 :below rows
                 :collect
                   (loop :for b :in basis
                         :collect (coerce (aref m x b) 'double-float)))))
    (make-array (list rows cols) 
      :initial-contents contents 
      :element-type 'double-float)))


(defun custom-divison (a b)
  (print a)
  (print b)
  (loop :for va :across a 
          :for vb :across b
          :collect (if (= vb 0d0) SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY (/ va vb))
  )
  )

(defun min-lista (estupido)
(loop
            :with min-tgt := SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY 
            :with min-idx := nil
            :for x :from 0
            :for tgt :in estupido
            :when (<= tgt min-tgt)
            :do (setf min-tgt tgt)
            :and
            :do (setf min-idx x)
            :finally (return min-idx)
          ))


(defun problem2 (path) 
  
   (loop    
   :for machine-manual :in (read-input path)
   :for buttons := (second machine-manual)
   :for jolts :=  (third machine-manual)
   
   :for rows := (length jolts)
   :for cols := (length buttons)
   
   :for vjolts := (make-array rows :initial-contents jolts)
    
   :for A := (button-array buttons rows cols)
   :for basis-and-remainder := (get-basis A)
   :for basis := (first basis-and-remainder)
   :for remainder := (second basis-and-remainder)
   :do (print machine-manual)
   :do (print (list "rows" rows))
   :do (print (list "cols" cols))
   :sum (loop
      :with cost := (make-array rows :initial-element 1d0 :element-type 'double-float)            
      :for B :=  (print (generate-basis A  basis))                  
      :for dummy := (print (list "Bdim" (array-dimensions B) "costdim" (array-dimensions cost)))
      :for invB := (lla:invert B)  
      
      :for u := (lla::mm cost invB)
         
      :for r := (loop for ai in remainder
                   :collect (list (- 1 (aref (lla:mm u (generate-basis A (list ai))) 0 )) ai))
      :for minr := (car (sort r #'< :key #'first))            
      :for yB := (lla:mm invB vjolts)
      :for cnt := (lla:mm cost yB)
      :when (or (not remainder) (>= (first minr) 0))
        :return cnt
      
      :do (let* (
              (aiv (generate-basis A (list (second minr))))
              (dB (lla:mm invB  aiv))
              (hm (custom-divison  yB (array-operations::flatten dB)))
              (replace-idx (min-lista hm))              
          )          


          (print dB)          
          (print yB)
          (print hm)
          (print basis)          
          (setf (nth replace-idx basis) (second minr))
          (print basis)
          (print (delete (second minr) remainder))
          

          )
      ;:do (print (list u r minr))      
      ;:return t
      ; :do (print (list buttons jolts (format-matrix A) basis (format-matrix B) 
      ;           "dimensions"
      ;           (array-dimensions B)
      ;           ;(array-dimensions vjolts)
      ;           (list A
      ;           vjolts
      ;           cost)
      ;           B
      ;           (lla:mm B vjolts)
      ;           r
      ;           ))
      
      )       
   ;:return t
  ))

(problem2 "2025/day10/test_input")
