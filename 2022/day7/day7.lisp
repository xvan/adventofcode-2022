(ql:quickload "fiveam")
(ql:quickload "draw-cons-tree")

;;; Tree primitives

(defun make-tree (data)
    (cons (cons data nil) nil))

(defun add-child (tree child) 
    ;(nconc (car tree) child)
    ;tree
    (rplaca tree (append (car tree) child))
    )

(defmacro first-child (tree)
    `(cdr (car ,tree)))

(defmacro next-sibling (tree)
    `(cdr ,tree))

(defmacro data (tree)
    `(car (car ,tree)))

(defun make-root () (make-tree nil))

(defun is-leaf (node)
    (not (listp (cdr node))))

;;; Processing

(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defmacro get-stack (tree-stack)
    `(cdr ,tree-stack))


(defmacro get-tree (tree-stack)
    `(car ,tree-stack))

(defun apply-pwd-node (f tree-stack)
    (let* ((tree (get-tree tree-stack)) (node tree))
         (dolist  (d (reverse (get-stack tree-stack)))
             (setf node (do ((nodeb (first-child node) (next-sibling nodeb))) ((equal (data nodeb) d) nodeb))))
         (funcall f node)
         tree
         )
    )

(defun apply-pwd-node (f tree-stack)
(do* ((stack (reverse (get-stack tree-stack)) (cdr stack)) (tree (get-tree tree-stack)) (node tree))
      ((not stack) (funcall f node) tree)
      (setf node (do ((child (first-child node) (next-sibling child))) ((equal (data child) (car stack)) child)))))


(let ((tree (make-tree nil)))
    (apply-pwd-node (lambda (n) (add-child n (make-tree "b"))) (cons tree nil))
    (apply-pwd-node (lambda (n) (add-child n (make-tree "c"))) (cons tree '("b")))
    tree
    )


(let ((tree (make-tree nil)))
    (apply-pwd-node (lambda (n) (add-child n (make-tree "c"))) (cons
    (apply-pwd-node (lambda (n) (add-child n (make-tree "b"))) (cons tree nil)) '("b"))))


(defun process-line (s tree-stack)
    (destructuring-bind (a b &optional c)  (split-by-one-space s)        
        (cond ((equal "$"  a) (rplacd tree-stack (process-command b c (get-stack tree-stack))))
              ((equal "dir" a) (add-directory tree-stack b))
              ((parse-integer a :junk-allowed t) (add-file tree-stack b (parse-integer a)))              
              )))

(defun process-command (cmd arg stack)
    (cond 
     ((equal "cd" cmd) (cond 
                        ((equal "/" arg) (setf stack nil))
                        ((equal ".." arg) (pop stack))
                        (t (push arg stack))))
     ((equal "ls" cmd) nil)
    )
    stack)

(defun add-directory (tree-stack name)
    (apply-pwd-node (lambda (n) (add-child n (make-tree name))) tree-stack))


(defun add-file (tree-stack name weight)
    (apply-pwd-node (lambda (n) (add-child n (make-tree (cons name weight)))) tree-stack))

(defun process-lines (lines tree-stack)
    (dolist (l lines)
        ;(print l)
        ;(terpri)
        (process-line l tree-stack))
    tree-stack)

(defun read-input (file)
  (with-open-file (in file)    
    (let ((stack `((:name ,nil :dir t :dlist ()))))
    (loop
        :for riv = (read-line in nil)
        :while riv
        :collect riv))))


(defun process-file (file)
    (process-lines (read-input file) (cons (make-tree nil) nil))    
    )

;;; Testing


(defparameter *test-tree* 
    (reduce 'add-child ( list
        (make-tree nil)
                        
        (reduce 'add-child ( list
            (make-tree "a")
            (reduce 'add-child ( list
                (make-tree "e")
                (make-tree (cons "i" 584))))
            (make-tree (cons "f" 29116))
            (make-tree (cons "g" 2557))
            (make-tree (cons "h.lst" 62596))))
                        
        (make-tree (cons "b.txt" 14848514))
        (make-tree (cons "c.dat" 8504156))
                            
        (reduce 'add-child ( list
            (make-tree "d")
            (make-tree (cons "j" 4060174))
            (make-tree (cons "d.log" 8033020))
            (make-tree (cons "d.ext" 5626152))
            (make-tree (cons "k" 7214296))))
        )))

;(draw-cons-tree:draw-tree  *test-tree*)

(fiveam:def-suite 7am-suite)
(fiveam:in-suite 7am-suite)
(fiveam:test test-change-directory
    (fiveam:is  (equal nil (get-stack (process-line "$ cd /" (cons nil nil) ))))
    (fiveam:is  (equal nil (get-stack (process-line "$ cd /" (cons nil '("e" "a")) ))))
    (fiveam:is  (equal '("a") (get-stack (process-line "$ cd .."  (cons nil '("e" "a"))))))
    (fiveam:is  (equal '("a" "d") (get-stack (process-line "$ cd .."  (cons nil '("e" "a" "d"))))))
    (fiveam:is  (equal '("a") (get-stack (process-line "$ cd a"  (cons nil nil)))))
    (fiveam:is  (equal '("b" "a") (get-stack (process-line "$ cd b"  (cons nil '("a"))))))    
    )

;(fiveam:test test-traverse
;    (fiveam:is  (equal *test-tree* (get-pwd-node (cons *test-tree* nil))))
;    (fiveam:is  (equal nil (get-pwd-node (cons *test-tree* '("e" "a"))))))

(fiveam:test test-add-directory
    (fiveam:is  (equal '((NIL ("a"))) (add-directory (cons (make-tree nil) nil) "a")))    
    (fiveam:is  (equal '((NIL ("a" ("b")))) (let ((tree (make-tree nil))) 
                                         (add-directory (cons tree nil) "a")
                                         (add-directory (cons tree '("a")) "b")))))

(fiveam:test test-add-file
    (fiveam:is  (equal '((NIL (("a.txt" . 100)))) (add-file (cons (make-tree nil) nil) "a.txt" 100))))



(fiveam:test test-parse-adds 
    (fiveam:is  (equal '((NIL ("a"))) (process-line "dir a" (cons (make-tree nil) nil))))
    (fiveam:is  (equal '((NIL ("a" ("b"))))  (process-line "dir b" (cons '((NIL ("a"))) '("a")))))
    (fiveam:is  (equal '((NIL (("a.txt" . 100)))) (process-line "100 a.txt" (cons (make-tree nil) nil)))))

(fiveam:test test-parse-lines
    (fiveam:is  (equal '(((NIL ("a" ("e")))) "a") (process-lines '("$ cd /"
                 "$ ls"
                 "dir a"
                 "$ cd a"
                 "$ ls"
                 "dir e") (cons (make-tree nil) nil))))

    (fiveam:is  (equal '(((NIL ("a") (("b.txt" . 14848514)) (("c.dat" . 8504156)))))
                      (process-lines '( "$ cd /" 
                                       "$ ls" 
                                       "dir a" 
                                       "14848514 b.txt" 
                                       "8504156 c.dat" ) (cons (make-tree nil) nil))))

    (fiveam:is  (equal *test-tree* (get-tree (process-file "day7/test"))))
    )


(defun is-file (node)
    (let ((datum (data node))) (and datum (not (atom datum)))))

(defun is-folder (node)
    (not (is-file node)))


(defun get-weight (node)    
        (if (is-folder node)             
            (loop :for child = (first-child node) :then (next-sibling child) 
                      :while child
                      :as weight = (get-weight child)                      
                      :when (is-folder child) :collect weight :into folderweight
                      :sum (car weight) :into currentsum
                      :finally (return (apply 'nconc (push (list currentsum) folderweight))))
            (cons (cdr (data node)) nil )))
            
(defun resolve-first (ws)
    (apply '+ (remove-if (lambda (w) (> w 100000)) (get-weight  ws))))

(defun resolve-second (ws) 
    (let ((weight (sort (get-weight  ws) '<))) 
        (find-if (lambda (c) (> (+ c (- 70000000 (apply 'max weight))) 30000000)) weight)))


(resolve-second (get-tree (process-file "day7/test")))

(fiveam:test test-weight 
    (fiveam:is  (equal '(100) (get-weight '((NIL (("a.txt" . 100)))))))
    (fiveam:is  (equal '(100) (get-weight (first-child '((NIL (("a.txt" . 100))))))))
    (fiveam:is  (equal '(48381165 94853 584 24933642) (get-weight  *test-tree*)))
    (fiveam:is  (equal 95437 (resolve-first  (get-tree (process-file "day7/test")))))
    (fiveam:is  (equal 1915606 (resolve-first  (get-tree (process-file "day7/input")))))
    (fiveam:is  (equal 24933642 (resolve-second  (get-tree (process-file "day7/test")))))
    (fiveam:is  (equal 1915606 (resolve-second  (get-tree (process-file "day7/input")))))
    )




(fiveam:run! '7am-suite)

 