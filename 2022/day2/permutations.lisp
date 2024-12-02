;;;CODE NOT USED IN SOLUTION

(ql:quickload "fiveam")

(defun permutations-b (l) 
    (if (= (length l) 1) (list l)
        (loop :for item in l         
        :collect (mapcar (lambda (b) (append (list item) b)) (permutations (remove item l))))
        ))

(defmacro with-gensyms ((&rest names) &body body)
    `(let ,(loop for n in names collect `(,n (gensym))) ,@body))


(defmacro default-if-nil (default &body body)     
    (with-gensyms (result-sym default-sym)
        `(let ((,result-sym (progn ,@body)) (,default-sym ,default) ) (if ,result-sym ,result-sym ,default-sym))))


(defun permutations (l)
    (default-if-nil 
            '(nil) 
        ( apply 'nconc (loop :for item in l 
                  :collect (mapcar (lambda (b) (append (list item) b)) (permutations (remove item l)))))))

(fiveam:test perm-1 (fiveam:is (equal '((1)) (permutations '(1)))))
(fiveam:test perm-2 (fiveam:is (equal '((1 2) (2 1)) (permutations '(1 2)))))
(fiveam:test perm-3 (fiveam:is (equal '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) (permutations '(1 2 3)))))



(fiveam:run!)
;(defun generate-keys () 
;    (mapcar (lambda (val) (hashpairs '(X Y Z) val)) (permutations '(A B C))))