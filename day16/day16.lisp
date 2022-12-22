(defun read-input (file)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :collect riv )))

(defun split-by-string (separator string)    
    (loop with l = (length separator)
          for i = 0 then (+ j l)
          as j = (search separator string :start2 i)          
          collect (subseq string i j)
          while j))


(defun get-between (string start-index start-token end-token)
    
    (let*
            (
             (start-token-length (length start-token))             
             (i (+ (search start-token string :start2 start-index) start-token-length))
             (j (if end-token (search end-token string :start2 i) (length string)))
             )
             (list i j)
             )        
        )



(defun clean-chunks (chunks)
    (destructuring-bind (node-chunk rate-chunk target-chunk) chunks 
    
    (list 
     :valve (read-from-string (string-trim '(#\e #\Space) node-chunk))
     :rate (read-from-string (string-trim '(#\e #\Space) rate-chunk))
     :neighbours (mapcar 'read-from-string (split-by-string ", " (string-trim '(#\e #\s #\Space) target-chunk)))
    )    
    ))
    

(defun parse-line (string)
    (loop 
     with n = 0
     for (start-token end-token) in  '(("Valve" "has") ("=" ";") ("valv" "nil"))
     for (start stop)=(get-between string n start-token end-token)    
     do (setf n stop)
     collect (subseq string start stop) into chunks
     finally (return (clean-chunks chunks))
    )               
)


(defun parse-file (file)
 (mapcar 'parse-line (read-input file))
 )


(defun resolve-symbols (input) 
    (loop
     with get-key = (lambda(b) (position b input :key (lambda(c) (getf c :valve))))
     for row in input
     collect (append row              
             `( :neighbour-index ,(mapcar get-key (getf row :neighbours))
                :id ,(funcall get-key (getf row :valve)))))    
        )


(defun make-graph (resolved-input)
    (let* ((len (length resolved-input))
           (graph (make-array `(,len ,len) :initial-element most-positive-fixnum)))
        (loop for row in resolved-input
              for id = (getf row :id)
              do (setf (aref graph id id) 0)
              do (loop for neighbour-id in (getf row :neighbour-index)
                    do (setf (aref graph id neighbour-id) 1)
                 )
        )
        (loop for k from 0 below len
            :do (loop for i from 0 below len
                    :do (
                             loop for j from 0 below len
                             for d = (+ (aref graph i k)(aref graph k j))
                             when (> (aref graph i j) d) do (setf (aref graph i j) d)                            
                        )))
        graph
    ))
    

(defun step-process-old (resolved visited graph time)
    (loop 
     with from-id = (getf (car visited) :id)
     for row in resolved
     for to-id = (getf row :id)
     for remain = (remove-if (lambda(y)(= to-id (getf y :id))) resolved )
     for consumed-time = (1+ (aref graph from-id to-id))
     for remaining-time = (- time consumed-time)
     for rate = (getf row :rate)
     for reward = (* remaining-time rate)
     for pseudo-current = (cons row visited)     
     maximize (+ reward (step-process remain pseudo-current graph remaining-time))
    )
)

(defun process-old (input)
    (let* (
           (resolved (resolve-symbols input))
           (graph-arr (make-graph resolved))
           )
           (step-process-old (remove-if (lambda(c)(= 0 (getf c :rate))) resolved) (list (car resolved)) graph-arr 30)
        ))

(defun step-costs (resolved visited graph time)
    (loop 
     with from-id = (getf (car visited) :id)
     for row in resolved
     for to-id = (getf row :id)
     ;for remain = (remove-if (lambda(y)(= to-id (getf y :id))) resolved )
     for consumed-time = (1+ (aref graph from-id to-id))
     for remaining-time = (- time consumed-time)
     for rate = (getf row :rate)
     for reward = (* remaining-time rate)
     ;for pseudo-current = (cons row visited)     
     collect `( :reward ,reward :remaining ,remaining-time :from-id ,from-id :to-id ,to-id :row ,row)
    )
)

(defun compare-remaining-time (a b)
    (cond ((> (getf a :remaining) (getf b :remaining)) t )
          ((< (getf a :remaining) (getf b :remaining)) nil )
          (t (>= (getf a :reward)    (getf b :reward)) )          
          ))

(defun compare-reward (a b)
    (cond ((> (getf a :reward) (getf b :reward)) t )
          ((< (getf a :reward) (getf b :reward)) nil )
          (t (>= (getf a :remaining) (getf b :remaining)))
          ))

(print "hello")
(defun step-candidates (resolved visited graph time) 
    (let* ((costs (step-costs resolved visited graph time))
           (costs-by-time (sort (copy-seq costs) 'compare-remaining-time ))
           (costs-by-reward (sort (copy-seq costs) 'compare-reward ))
          )
          (cond
           ((null costs) nil)
           ((equal (car costs-by-reward) (car costs-by-time)) (list (car costs-by-reward)))
           (t (remove-duplicates (append
             (subseq costs-by-reward 0 (position (car costs-by-reward) costs-by-time :test 'equal))
             (subseq costs-by-time 0 (position (car costs-by-time) costs-by-reward :test 'equal)))))
          )))

(defun step-process (resolved visited graph time)
    (loop
     for candidate in (step-candidates resolved visited graph time)
     for remain = (remove-if (lambda(y)(= (getf candidate :to-id) (getf y :id))) resolved )
     for pseudo-current = (cons (getf candidate :row) visited)
     for reward = (getf candidate :reward)
     for remaining-time = (getf candidate :remaining)
     do (incf *global-count*)
     maximize (+ reward (step-process remain pseudo-current graph remaining-time))
     )
)

(defvar *global-count* 0)

(defun process (input)
    (let* (
           (resolved (resolve-symbols input))
           (graph-arr (make-graph resolved))
           )
        (setf *global-count* 0)
        (step-process (remove-if (lambda(c)(= 0 (getf c :rate))) resolved) (list (car resolved)) graph-arr 30)      
        ))

(defun struct-input (input)
    (let* (
           (resolved (resolve-symbols input))
           (graph-arr (make-graph resolved))           
           (candidates (remove-if (lambda(c)(= 0 (getf c :rate))) resolved))
           (start-node (car resolved))           
           )
        (list :graph graph-arr :candidates candidates :start-node start-node)
    ))




(defun make-calc-remaining (graph)
    (lambda (from-node to-node time)
        (let*
        ((from-id (getf from-node :id))
         (to-id  (getf to-node :id))        
         (consumed-time (1+ (aref graph from-id to-id)))
         (remaining-time (- time consumed-time))
        )
            remaining-time
        )))

(defun calc-reward (node time)
    (* (getf node :rate) (max 0 time)))

(defun make-bound-tester (calc-remaining-lambda)
    (lambda (step-state)
         (destructuring-bind (&key node reward candidates time &allow-other-keys)  step-state
                (loop
                    for candidate in candidates
                    for remaining-time = (funcall calc-remaining-lambda node candidate time)                    
                        sum (calc-reward candidate remaining-time) into reward-bound
                    finally (return (+ reward-bound reward))
                ))))


(defun make-step-plist (node reward candidates time)
    (list :node node :reward reward :candidates candidates :time time))

(defun make-next-leaves-lambda (graph)
    (let* ((calc-remaining-lambda (make-calc-remaining graph))
           (bound-tester-lambda (make-bound-tester calc-remaining-lambda))           
           )
        (lambda (step-state)
            (destructuring-bind (&key node reward candidates time)  step-state                
                (loop                 
                 for candidate in candidates                 
                 for remaining-candidates =  (remove-if (lambda(y)(equal candidate y)) candidates )
                 for remaining-time = (funcall calc-remaining-lambda node candidate time)
                 for candidate-reward = (+ reward (calc-reward candidate remaining-time))
                 for candidate-step = (make-step-plist candidate candidate-reward remaining-candidates remaining-time)
                 for candidate-ub = (funcall bound-tester-lambda candidate-step)
                 collect (make-leaf candidate-step candidate-ub)
                 )
            )
        )
    )
)


(defun make-leaf (step ub)
    (list :step step :ub ub))

(defun process-bb (input)
(destructuring-bind (&key graph candidates start-node &allow-other-keys) (struct-input input)  
    (let* (
           (next-leaves-lambda (make-next-leaves-lambda graph))
           (start-step (make-step-plist start-node 0 candidates 30))
           ;(leaves (list start-step))
            )
            (do* (                 
                 (best-path most-negative-fixnum)
                 (leaves (list))
                 (current-step start-step (getf (pop leaves) :step))
                 (n 0 (1+ n))
                 ) 
                ((null current-step) (list best-path n))
                (setf leaves (nconc leaves (funcall next-leaves-lambda current-step)))                
                (setf best-path   (apply 'max (cons best-path (mapcar (lambda(l) (getf (getf l :step ) :reward)) leaves ))))
                (delete-if (lambda (l) (> best-path (getf l :ub)))leaves)
                (sort leaves '> :key (lambda (l) (getf l :ub)))
                )              
            )))
           

(process-bb (parse-file "day16/test"))

(process (parse-file "day16/test"))

(process-bb (parse-file "day16/input"))

;(make-array '(4 4) :initial-contents '((0 1 1 2) (1 0 1 1) (1 1 0 1) (2 1 1 0)) ) 


(ql:quickload "fiveam")

(fiveam:def-suite 16am-suite)
(fiveam:in-suite 16am-suite)

(fiveam:test test-parser
    (fiveam:is (equal '(:valve JJ :rate 21 :neighbours (II)) (parse-line "Valve JJ has flow rate=21; tunnel leads to valve II")))
    (fiveam:is (equal '(:valve DD :rate 20 :neighbours (CC AA EE)) (parse-line "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE")))    
)

(fiveam:test test-graph
    ;(fiveam:is (equal (make-array '(4 4) :initial-contents '((0 1 1 2) (1 0 1 1) (1 1 0 1) (2 1 1 0)) ) (make-graph '((:valve A :neighbours (B C)) (:valve B :neighbours (A D)) (:valve C :neighbours (A D)) (:valve D :neighbours (B C)) ))))    
)

(fiveam:test test-process
    ;(fiveam:is (equal 1651 (process (parse-file "day16/test"))))
    ;(fiveam:is (equal 1651 (process (parse-file "day16/input"))))
)

(fiveam:run! '16am-suite)