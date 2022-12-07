(ql:quickload "fiveam")

(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


(defun process-line (s stack)
    (destructuring-bind (a b &optional c)  (split-by-one-space s)
        (cond ((equal "$"  a) (process-command b c stack))
              ((equal "dir" a) (add-directory (car stack) b))
              ((parse-integer a :junk-allowed t) (add-file (car stack) b (parse-integer a)))
              )))

(defun process-command (cmd arg stack)
    (cond 
     ((equal "cd" cmd) (cond 
                        ((equal "/" arg) (loop                                                                                    
                                          (when (not (getf (car stack) :name)) (return))
                                          (pop stack)))
                        ((equal ".." arg) (pop stack))
                        (t (push (find-if (lambda (d) (and (getf d :dir) (equal arg (getf d :name))))  (getf (car stack) :dlist) ) stack ))))
     ((equal "ls" cmd) nil)
    )
    (car stack))

(defun add-directory (pwd name) 
    (push `(:name ,name :dir t :dlist ()) (getf pwd :dlist))
    pwd)

(defun add-file (pwd name weight) 
    (push `(:name ,name :dir nil :weight ,weight) (getf pwd :dlist))
    pwd)

(defun read-input (file)
  (with-open-file (in file)    
    (let ((stack `((:name ,nil :dir t :dlist ()))))
    (loop
        :for riv = (read-line in nil)
        :while riv
        :do (process-line riv stack)
        :finally (return stack)))))




(fiveam:test test-change-directory
    (fiveam:is  (equal `(:name ,nil :dir t) (process-line "$ cd /" `((:name ,nil :dir t))) ))
    (fiveam:is  (equal `(:name ,nil :dir t) (process-line "$ cd /" `((:name a :dir t)(:name b :dir t)(:name ,nil :dir t)) )))
    (fiveam:is  (equal '(:name b :dir t) (process-line "$ cd .." `((:name a :dir t)(:name b :dir t)(:name ,nil :dir t))) ))
    (fiveam:is  (equal `(:name ,nil :dir t) (process-line "$ cd .." `((:name b :dir t)(:name ,nil :dir t))) ))
    (fiveam:is  (equal `(:name "pepe" :dir t :dlist ((:name juan :dir f :weight 10))) (process-line "$ cd pepe" `((:name ,nil :dir t :dlist ((:name "pepe" :dir t :dlist ((:name juan :dir f :weight 10))))))))))

(fiveam:test test-add-directory
    (fiveam:is  (equal `(:name "base" :dir t :dlist ((:name "hijo" :dir t :dlist ())) ) (add-directory `(:name "base" :dir t :dlist ()) "hijo"))))

(fiveam:test test-add-file
    (fiveam:is  (equal `(:name "base" :dir t :dlist ((:name "hijo" :dir nil :weight 1000)) ) (add-file `(:name "base" :dir t :dlist ()) "hijo" 1000))))

(fiveam:test test-parse-adds 
    (fiveam:is  (equal `(:name ,nil :dir t :dlist ((:name "pepe" :dir t :dlist ()))) (process-line "dir pepe" `((:name ,nil :dir t :dlist ()))) ))
    (fiveam:is  (equal `(:name ,nil :dir t :dlist ((:name "hijo" :dir nil :weight 1000))) (process-line "1000 hijo" `((:name ,nil :dir t :dlist ()))) ))
    )

(fiveam:test test-input-first (fiveam:is-true t))


(read-input "day7/test")
;(fiveam:run!)