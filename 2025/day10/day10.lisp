;(problem1 "2025/day10/input")      

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


; (defun generate-by-bitcount (n)
;   "Return a list of all integers 0..(2^n - 1)
;    ordered by increasing number of bits set."
;   (let ((limit (ash 1 n))
;         (result '()))
;     (push 0 result)
;     (loop for k from 1 to n do
;       (let ((v (1- (ash 1 k))))   ; smallest number with k bits: 0...011..1
;         (loop while (< v limit) do
;           (push v result)
;           (setf v (or (gospers-next v) limit)))))
;     (nreverse result)))

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

(mapcar #'bits-on (generate-by-kbitcount 4 2))

(bits-on 0)


(defun print-bin (n bits)
  (format t "~v,'0b~%" bits n))

(mapc (lambda (x) (print-bin x 4))
      (generate-by-bitcount 4))



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
  

(defun generate-basis (m basis)
  (let* ((rows (array-dimension m 0))
         (cols (length basis))
         (contents
           (loop :for x :from 0 :below rows
                 :collect
                   (loop :for b :in basis
                         :collect (coerce (aref m x b) 'double-float)))))
    (make-array (list rows cols) :initial-contents contents :element-type 'double-float)))


(defun problem2 (path) 
  
   (loop    
   :for machine-manual :in (read-input path)
   :for buttons := (second machine-manual)
   :for jolts :=  (third machine-manual)
   :for vjolts := (make-array (length jolts) :initial-contents jolts)
   :for rows := (length jolts)
   :for cols := (length buttons)   
   :for A := (button-array buttons rows cols)
   :for basis := (get-basis A)
   :for B := (generate-basis A (first basis))
   :for invB := (lla:invert B)
   :for cost := (make-array rows :initial-element 1d0 :element-type 'double-float)
   
   :for u := (lla::mm cost invB)

   :for r := (loop for ai in (second basis)
                   :collect (- 1 (aref (lla:mm u (generate-basis A (list ai))) 0 ))
                          ;  (lla:mm
                          ;  (cl-num-utils.matrix:transpose u) 
                          ;  (generate-basis A (list ai)))
                           
                   )


   :do (print (list buttons jolts (format-matrix A) basis (format-matrix B) 
                "dimensions"
                (array-dimensions B)
                ;(array-dimensions vjolts)
                B
                (lla:mm B vjolts)                
                r
                ))    
   :return t
  ))

(problem2 "2025/day10/test_input")

;; Now inversion works
(defparameter A-inv (lla:invert A))



(ql:quickload "lla")

(defparameter *matrix-a* (make-array '(2 2) :initial-contents '((1.0 2.0) (3.0 4.0))))
(defparameter *vector-b* (make-array '(2) :initial-contents '(5.0 6.0)))
