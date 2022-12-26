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
    

(defun struct-input (input)
    (let* (
           (resolved (resolve-symbols input))
           (graph-arr (make-graph resolved))           
           (candidates (remove-if (lambda(c)(= 0 (getf c :rate))) resolved))
           (start-node (find 'AA resolved :test 'equal :key (lambda(x)(getf x :valve))))           
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
         (destructuring-bind (&key workers (reward 0) candidates &allow-other-keys)  step-state
                (loop
                    for candidate in candidates
                    for remaining-time = (apply 'max (mapcar (lambda (w) 
                                          (destructuring-bind (&key node time &allow-other-keys) w
                                            (funcall calc-remaining-lambda node candidate time))) workers))
                        sum (calc-reward candidate remaining-time) into reward-bound
                    finally (return (+ reward-bound reward))
                ))))

(defun make-worker-plist (node time previous)
    (list :node node :time time :previous previous) )

(defun make-step-plist (workers reward candidates)
    (list :workers workers :reward reward :candidates candidates))

(defun make-next-leaves-lambda (graph)
    (let* ((calc-remaining-lambda (make-calc-remaining graph))
           (bound-tester-lambda (make-bound-tester calc-remaining-lambda))           
           )
        (lambda (step-state)
            (destructuring-bind (&key workers (reward 0) candidates &allow-other-keys)  step-state
                (destructuring-bind (&key node (time 0) &allow-other-keys) (car workers)
                    (loop                 
                        for candidate in candidates                 
                        for remaining-candidates =  (remove-if (lambda(y)(equal candidate y)) candidates )
                        for remaining-time = (funcall calc-remaining-lambda node candidate time)
                        for candidate-reward = (+ reward (calc-reward candidate remaining-time))                                            
                        for new-worker = (make-worker-plist candidate remaining-time nil) ;(car workers)                     
                        for new-workers = (sort (append (list new-worker) (copy-list (cdr workers))) '> :key (lambda (w) (getf w :time)))
                        for candidate-step = (make-step-plist new-workers candidate-reward remaining-candidates)
                        for candidate-ub = (funcall bound-tester-lambda candidate-step)
                        collect (make-leaf candidate-step candidate-ub)                     
                    )
                )
            )
        )
    )
)


(defun make-leaf (step ub)
    (list :step step :ub ub))

(defun process-bb (input start-time n-workers)
(destructuring-bind (&key graph candidates start-node &allow-other-keys) (struct-input input)  
    (let* (
           (next-leaves-lambda (make-next-leaves-lambda graph))
           (start-worker (make-worker-plist start-node start-time nil))
           (start-step (make-step-plist (make-list n-workers :initial-element start-worker) 0 candidates))
           (ub-key (lambda (l) (getf l :ub)))
           ;(leaves (list start-step))
            )
            (do* (                 
                 (best-path most-negative-fixnum)
                 (best-step nil)
                 (leaves (list))                 
                 (current-step start-step (getf (pop leaves) :step))
                 (n 0 (1+ n))
                 ) 
                ((null current-step) (list best-path n best-step))
                ;(print current-step)
                (setf leaves
                    (merge 'list leaves 
                       (sort (funcall next-leaves-lambda current-step) '> :key ub-key)
                       '> :key ub-key))
                ;(print (list :leaves leaves))

                (let ((best-leave (when leaves (reduce (lambda (a b) (if (> (getf (getf b :step ) :reward) (getf (getf a :step ) :reward)) b a)) leaves ))))
                    (when (and best-leave (> (getf (getf best-leave :step) :reward) best-path))
                        (setf best-step (getf best-leave :step) )
                        (setf best-path (getf best-step :reward) )      
                    )                                    
                )
                    (setf leaves (subseq leaves 0 (position-if (lambda (l) (> best-path (getf l :ub))) leaves)))
                ))
    ))          


(defun show-result (result)
    (destructuring-bind (reward iterations best-step) result
        (print (list :reward reward))
        (print (list :iterations iterations))
        (loop for step = best-step then (getf step :previous)
        when (null step) do (return (reverse acc))
        collect (list :valve (getf (getf step :node) :valve) :time (getf step :time) :reward (getf step :reward)) into acc
        )        
    ))


;(show-result (process-bb (parse-file "day16/test") 30 1))

(defun make-graphviz (input)
    (format t "~{<node id='~a'/>~%~}" (mapcar (lambda (x) (getf x :valve)) input))
    (format t "~{~{<edge source='~a' target='~a'/>~%~}~}"(apply 'append (mapcar (lambda (x) (mapcar (lambda (y) (list (getf x :valve) y)) (getf x :neighbours) ) ) input)))    
    )

;(make-graphviz (parse-file "day16/input"))

;(make-array '(4 4) :initial-contents '((0 1 1 2) (1 0 1 1) (1 1 0 1) (2 1 1 0)) ) 

(process-bb (parse-file "day16/input") 26 2)

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
    (fiveam:is (= 1651 (first (process-bb (parse-file "day16/test") 30 1))))
    (fiveam:is (= 2087 (first (process-bb (parse-file "day16/input") 30 1))))    
    (fiveam:is (= 1651 (first (process-bb (parse-file "day16/test") 30 2))))
    ;warining slow
    ;(fiveam:is (= 2591 (first (process-bb (parse-file "day16/input") 30 2))))
    
)

(fiveam:run! '16am-suite)