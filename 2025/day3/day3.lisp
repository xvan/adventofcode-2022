(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun read-input (file)
  (with-open-file (in file)
  (loop          
                  :for riv = (read-line in nil)                   
                  :while riv                  
                  :collect (mapcar (lambda (c) (- (char-code c) (char-code #\0))) (str-to-list riv)) )))


(defun enumerate (l) 
      (loop
       :for x :in l
       :for n :from 0
       :collect (list n x)
       )
      )



(defun problem1 (path)
( loop 
 :for bank :in  (read-input path)
 ;:collect
 :sum
      (let* (
             (n (1- (length bank)))
             (sorted-bank (sort (enumerate bank) #'(lambda (x y) 
                                                         (if (= (second x) (second y))
                                                            (< (first x) (first y))
                                                             (> (second x) (second y))                                                         
                                                         ))))
             (first-battery (first (remove-if #'(lambda (x) (eq n (first x))) sorted-bank)))
             (second-battery (first (remove-if #'(lambda (x) (< (first x) (1+ (first first-battery)))) sorted-bank)))
             )                        
            ;(list first-battery second-battery)            
            (+ (second second-battery) (* 10 (second first-battery)))      
      )
))

(problem1 "2025/day3/test_input")

(problem1 "2025/day3/input")

(defun problem2 (path)
( loop 
 :for bank :in  (read-input path)
 :sum
      (reduce #'(lambda (x y) (+ y (* 10 x))) 
      (loop 
             :with n :=  (length bank)
             :with sorted-bank :=  (sort (enumerate bank) #'(lambda (x y) 
                                                         (if (= (second x) (second y))
                                                            (< (first x) (first y))
                                                             (> (second x) (second y))                                                         
                                                         )))
            :with last-battery-idx := -1
            :for b :from 12 :downto 1
            :for current-battery := (first (identity (
                               remove-if #'(lambda (x) (>= (first x) (- n (1- b)) )) 
                                    (remove-if #'(lambda (x) (<= (first x) last-battery-idx)) sorted-bank))))
            :do (setf last-battery-idx (first current-battery))
            :collect (second current-battery)
             )                       
    
      )
)
)

(problem2 "2025/day3/test_input")
(problem2 "2025/day3/input")