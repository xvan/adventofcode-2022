(ql:quickload "cl-ppcre")


(defun read-input (file)
  (with-open-file (in file)
  (values-list 
  (list 
  (loop          
                  :for riv = (read-line in nil)                   
                  :while (and riv (not (equalp riv "")) )
                  :collect (mapcar #'parse-integer (cl-ppcre:split "-" riv))
  )
            (loop          
                  :for riv = (read-line in nil)                   
                  :while (and riv (not (equalp riv "")) )
                  :collect (parse-integer riv)
  )))))

                  ;:append (mapcar #'(lambda (x) (mapcar #' parse-integer(cl-ppcre:split "-" x))) (cl-ppcre:split "," riv ));(mapcar #'parse-integer (cl-ppcre:split "," riv ))



(defun sort-ranges (ranges)
      (sort ranges #'(lambda (x y) (if (eq (first x) (first y)) (< (second x) (second y)) (< (first x) (first y)))))
      )

(defun collapse-ranges (sorted-ranges)
      (loop        
       :with current := (car sorted-ranges)       
       :for next :in (cdr sorted-ranges)
       :for x :from 0
       :do (identity (list x current next))
       :when (>= (second current) (first next))
       :do (setq current (list (first current) (max (second current) (second next))))
       :else
       :collect (let ((cval current))
                (setq current next)
                (identity cval)) :into aux
       :finally
       (return (nconc aux (list current)))
      )      
)
      
(defun find-fresh-ids (fresh-ranges ids)
      (let ((collapsed (collapse-ranges (sort-ranges fresh-ranges))))
      (remove-if-not #'(lambda (d) (some #'(lambda (r) (and (>= d (first r)) (<= d (second r)))) collapsed)) ids)
      )
      )      

(defun problem1 (path)
     (multiple-value-bind (fresh-ranges ids) (read-input path)
            (length (find-fresh-ids fresh-ranges ids))
      ))

(problem1 "2025/day5/test_input")
(problem1 "2025/day5/input")


(defun problem2 (path)
     (multiple-value-bind (fresh-ranges ids) (read-input path)
            ids
            (reduce '+ (mapcar #'(lambda (x) (1+ (- (second x) (first x)))) (collapse-ranges (sort-ranges fresh-ranges))))
      ))


(problem2 "2025/day5/test_input")

(problem2 "2025/day5/input")