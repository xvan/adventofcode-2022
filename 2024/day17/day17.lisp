(ql:quickload "cl-ppcre")

(defun read-input (file)
  (with-open-file (in file)
  (let ((A 0) (B 0) (C 0) (IP 0) (PROG nil))
      (setq A (parse-integer (second (cl-ppcre:split "(:\\s)" (read-line in nil) ))))
      (setq B (parse-integer (second (cl-ppcre:split "(:\\s)" (read-line in nil) ))))
      (setq C (parse-integer (second (cl-ppcre:split "(:\\s)" (read-line in nil) ))))
      (read-line in nil)
      (setq PROG (mapcar #'parse-integer (cl-ppcre:split "," (second (cl-ppcre:split "(:\\s)" (read-line in nil) )))))
      (list A B C IP (make-array (length PROG) :initial-contents PROG))
      ))
  )


(defun PROC (A B C IP OUT PROG)
  (let* (
        (OPCD (aref PROG IP))
        (OPRN (aref PROG (1+ IP)))
        (CMB (nth OPRN (list 0 1 2 3 A B C))))
    (incf IP 2)
    ;(format t "OPCD:~a OPRN:~a CMB:~a~%" OPCD OPRN CMB)
    (cond
     ((= OPCD 0) (setf A (ash A (- CMB))))
     ((= OPCD 1) (setf B (logxor B OPRN)))
     ((= OPCD 2) (setf B (mod CMB 8)))
     ((= OPCD 3) (when (/= A 0) (setq IP OPRN)))
     ((= OPCD 4) (setf B (logxor B C)))
     ((= OPCD 5) (PUSH (mod CMB 8) OUT))
     ((= OPCD 6) (setf B (ash A (- CMB))))
     ((= OPCD 7) (setf C (ash A (- CMB))))
     )
    (list A B C IP OUT)
    )
  )

; (defun solve-first (file) 
;   (destructuring-bind (A0 B0 C0 IP0 PROG) (read-input file)
;     (format nil "~{~a~^,~}" 
;       (loop
;        :for (A B C IP OUT) := (PROC A0 B0 C0 IP0 nil PROG) :then (PROC A B C IP OUT PROG)
;        :while (< IP (array-dimension PROG 0))
;        :finally (return (reverse OUT))
;       )
;     )
; ))



(defun exec-prog (A0 B0 C0 IP0 PROG) 
    
      (loop
       :for (A B C IP OUT) := (PROC A0 B0 C0 IP0 nil PROG) :then (PROC A B C IP OUT PROG)
       :while (< IP (array-dimension PROG 0))
       :finally (return (reverse OUT))
      )
    )

(defun solve-first (file)
  (format nil "~{~a~^,~}" 
  (apply #'exec-prog (read-input file))
  )
  ) 


(defun prev-inst (IP PROG)
  (mod (- IP 2) (length PROG)))

(defun current-out (PROG OUT)
  (aref PROG (- (length PROG) (length OUT) 1)))


(defun reverse-out (A B C IP OUT PROG)
  (let* (
        (OPRN (aref PROG (1+ IP)))
        (REGS (make-array 7 :initial-contents (list '(0) '(1) '(2) '(3) A B C)))
        (CMB (aref REGS OPRN))
        (TGT (current-out PROG OUT))
        (CMB-CANDIDATE (remove-if-not (lambda (x) (= (mod x 8) TGT)) CMB))
    )
    (format t "CMB:~a CANIDATE:~a TGT:~a~%" CMB cmb-candidate TGT)
    (IF CMB 
        (WHEN CMB-CANDIDATE
              (setf (aref REGS OPRN) CMB-CANDIDATE)
              (when-not-ended (reverse-proc (aref REGS 4) (aref REGS 5) (aref REGS 6) (prev-inst IP PROG) (CONS TGT OUT) PROG)))
        (loop :for n :from 0
              :for limit :from 0 :below 10
              :for dummy := (SETF (aref REGS OPRN)  (list (+ (* 8 n) TGT)))
              :for res := (when-not-ended (reverse-proc (aref REGS 4) (aref REGS 5) (aref REGS 6) (prev-inst IP PROG) (CONS TGT OUT) PROG))
              :when res
              :return res
        ))
      ))

(defmacro getA (REGS) `(aref ,REGS 4))
(defmacro getB (REGS) `(aref ,REGS 5))
(defmacro getC (REGS) `(aref ,REGS 6))

(defmacro when-not-ended (body)
  `(if (and (= IP 0) (= (length OUT) (length PROG)))
        (when (loop :for i :from 0
                      :for x :in OUT
                      :always (= x (aref PROG i))
                      ) (first (getA REGS))) 
        ,body))

(defun reverse-xdv (A B C IP OUT PROG RIDX)
  (let* ((OPRN (aref PROG (1+ IP)))
         (REGS (make-array 7 :initial-contents (list '(0) '(1) '(2) '(3) A B C)))
         (CMB (aref REGS OPRN))
         )
    (when (not CMB) (error "infinite loop imp pending"))
    (when (= OPRN 4) (error "shifting A with A"))

    (setf (aref REGS RIDX) 
        (remove-duplicates (sort
                (loop :for c :in CMB :append
                        (loop for ca :in A :append 
                                (loop 
                                 :with sA := (ash ca c)
                                 :for n :from 0 :below (ash 1 c)
                                 :collect (+ sA n)
     )))#'< )))

    (when-not-ended
        (reverse-proc  (getA REGS) (getB REGS) (getC REGS) (prev-inst IP PROG) OUT PROG))
    ))

(defun reverse-adv (A B C IP OUT PROG)
  (reverse-xdv A B C IP OUT PROG 4)
  )

(defun reverse-bdv (A B C IP OUT PROG)
  (reverse-xdv A B C IP OUT PROG 5)
  )

(defun reverse-cdv (A B C IP OUT PROG)
  (reverse-xdv A B C IP OUT PROG 6)
  )

; (defun reverse-adv (A B C IP OUT PROG)
;   (let* ((OPRN (aref PROG (1+ IP)))
;          (REGS (make-array 7 :initial-contents (list '(0) '(1) '(2) '(3) A B C)))
;          (CMB (aref REGS OPRN))
;          (A-CANDIDATES nil)
;          )
;     (when (not CMB) (error "infinite loop imp pending"))
;     (when (= OPRN 4) (error "shifting A with A"))
;     (setq A-CANDIDATES 
;         (remove-duplicates (sort
;                 (loop :for c :in CMB :append
;                         (loop for ca :in A :append 
;                                 (loop 
;                                  :with sA := (ash ca c)
;                                  :for n :from 0 :below (ash 1 c)
;                                  :collect (+ sA n)
;      )))#'< )))
;     (if (= (length OUT) (length PROG))
;         (when (loop :for i :from 0
;                       :for x :in OUT
;                       :always (= x (aref PROG i))
;                       ) (first A-CANDIDATES)) 
;         (reverse-proc A-CANDIDATES B C (prev-inst IP PROG) OUT PROG))))

; (defun reverse-bdv (A B C IP OUT PROG)
;   (let* ((OPRN (aref PROG (1+ IP)))
;          (REGS (make-array 7 :initial-contents (list '(0) '(1) '(2) '(3) A B C)))
;          (CMB (aref REGS OPRN))
;          (A-CANDIDATES nil)
;          )
;     (when (not CMB) (error "infinite loop imp pending"))
;     (setq A-CANDIDATES 
;         (remove-duplicates (sort
;                 (loop :for c :in CMB :append
;                         (loop for ca :in B :append 
;                                 (loop 
;                                  :with sA := (ash ca c)
;                                  :for n :from 0 :below (ash 1 c)
;                                  :collect (+ sA n)
;      )))#'< )))
;     (if (AND (= 0 IP) (= (length OUT) (length PROG)))
;         (when (loop :for i :from 0
;                       :for x :in OUT
;                       :always (= x (aref PROG i))
;                       ) (first A-CANDIDATES)) 
;         (reverse-proc A-CANDIDATES B C (prev-inst IP PROG) OUT PROG))))

; (defun reverse-cdv (A B C IP OUT PROG)
;   (let* ((OPRN (aref PROG (1+ IP)))
;          (REGS (make-array 7 :initial-contents (list '(0) '(1) '(2) '(3) A B C)))
;          (CMB (aref REGS OPRN))
;          (A-CANDIDATES nil)
;          )
;     (when (not CMB) (error "infinite loop imp pending"))
;     (setq A-CANDIDATES 
;         (remove-duplicates (sort
;                 (loop :for c :in CMB :append
;                         (loop for ca :in C :append 
;                                 (loop 
;                                  :with sA := (ash ca c)
;                                  :for n :from 0 :below (ash 1 c)
;                                  :collect (+ sA n)
;      )))#'< )))
;     (if (= (length OUT) (length PROG))
;         (when (loop :for i :from 0
;                       :for x :in OUT
;                       :always (= x (aref PROG i))
;                       ) (first A-CANDIDATES)) 
;         (reverse-proc A-CANDIDATES B C (prev-inst IP PROG) OUT PROG))))

(defun reverse-bxl (A B C IP OUT PROG)
  (let* ((OPRN (aref PROG (1+ IP)))
         (REGS (make-array 7 :initial-contents (list '(0) '(1) '(2) '(3) A B C)))
         )
  (when-not-ended (reverse-proc A (mapcar (lambda (x) (logxor x OPRN)) B) C  (prev-inst IP PROG) OUT PROG)))
)

(defun reverse-bxc (A B C IP OUT PROG)
  (let* ((OPRN (aref PROG (1+ IP)))
         (REGS (make-array 7 :initial-contents (list '(0) '(1) '(2) '(3) A B C)))
         )
  (when-not-ended (reverse-proc A 
                                  (loop :for sb :in B 
                                    :append (loop :for sc :in C
                                                          :collect (logxor sb sc)
                                                          )
                                          )  C  (prev-inst IP PROG) OUT PROG))))
  ;setf B (logxor B OPRN))

(defun reverse-bst (A B C IP OUT PROG)
(let* (
        (OPRN (aref PROG (1+ IP)))
        (REGS (make-array 7 :initial-contents (list '(0) '(1) '(2) '(3) A B C)))
        (CMB (aref REGS OPRN))
    )
    (when (not B) (error "implementation pending"))

    (loop :for n :from 0
          :for limit :from 0 :below 10
          :for dummy := (SETF (aref REGS OPRN)  
                          (loop :for sb :in B
                              :collect (+ (* 8 n) sb)))
          :for res := (when-not-ended (reverse-proc (getA REGS) (getB REGS) (getC REGS) (prev-inst IP PROG) OUT PROG))
          :when res
          :return res
    )
      ))

(defun reverse-proc (A B C IP OUT PROG)
  (let* (
        (OPCD (aref PROG IP))
        (OPRN (aref PROG (1+ IP)))
        (CMB (nth OPRN (list '(0) '(1) '(2) '(3) A B C))))
        
    (format t "OPCD:~a OPRN:~a CMB:~a~%" OPCD OPRN CMB)
    (list A B C IP OUT PROG)
    (cond
    ((= OPCD 0) (reverse-adv A B C IP OUT PROG))
    ((= OPCD 1) (reverse-bxl A B C IP OUT PROG))
    ((= OPCD 2) (reverse-bst A B C IP OUT PROG))
    ((= OPCD 3) (reverse-proc A B C (prev-inst IP PROG) OUT PROG))
    ((= OPCD 4) (reverse-bxc A B C IP OUT PROG))
    ((= OPCD 5) (reverse-out A B C IP OUT PROG))
    ((= OPCD 6) (reverse-bdv A B C IP OUT PROG))
    ((= OPCD 7) (reverse-cdv A B C IP OUT PROG))
    (T nil)
    )
    )
)

(reverse-proc '(0) nil nil 4 nil #(0 3 5 4 3 0))

(reverse-proc '(0) nil nil 14 nil #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0))
; (current-out  #(0 3 5 4 3 0) '(0))


;(trace reverse-proc)
;(trace reverse-adv)

(defun reverse-first (file)
  (destructuring-bind (A0 B0 C0 IP0 PROG) (read-input file)
    (list A0 B0 C0 IP0)
    (reverse-proc 0 nil nil (- (length PROG) 2) nil PROG ))
  )




(defun partial-proc (A0 B0 C0 IP0 PROG) 
  (loop
       :for (A B C IP OUT) := (PROC A0 B0 C0 IP0 nil PROG) :then (PROC A B C IP OUT PROG)
       :for halt := (not (< IP (array-dimension PROG 0)))
       :while (and (not OUT) (not halt))
       :finally (return (list A B C IP OUT halt))
      ))


(defun is-corrupt (A0 B0 C0 IP0 PROG TGT) 
  (loop  
        :for p :in TGT 
        :for (A B C IP OUT halt) := (partial-proc A0 B0 C0 IP0 PROG) :then  (partial-proc A B C IP PROG)
        :when (or halt (/= (car OUT) p))
        :return t
        :finally (return (not (nth 5 (partial-proc A B C IP PROG)))) ))


  (destructuring-bind (A0 B0 C0 IP0 PROG) (read-input "2024/day17/test_input1")
   (list A0 B0 C0 IP0 PROG)
   (is-corrupt 2024 B0 C0 0 PROG))
 
(untrace partial-proc)
(untrace proc)

(defun solve-second-bruteforce (file) 
  (destructuring-bind (A0 B0 C0 IP0 PROG) (read-input file)
    (list IP0 A0)
    (loop
      :with TGT := '(1 7 4 1 5 5 3 0)
      :for At :from 0
      :while (is-corrupt At B0 C0 0 PROG TGT)
      :do (when (= 0 (mod At 1000000)) (print At))
      :finally (return At)
      )))


(solve-second-bruteforce "2024/day17/input")


;(solve-second "2024/day17/test_input1")
;solve-second "2024/day17/input")


; (solve-first "2024/day17/test_input0")
(solve-first "2024/day17/test_input1")
(solve-first "2024/day17/input")



( 
 loop
 :with PROG := #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)
 :for a0 :from 0 :below 100
 :when
  (print (loop
    ;:with PROG := #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)
    :for last-ip := -1 :then IP
    :for (A B C IP OUT) := (PROC a0 nil nil 0 nil PROG) :then (PROC A B C IP OUT PROG)
    ;:do (format t "A=~a B=~a C=~a IP=~a OUT=~a~%" A B C IP OUT)
    :while  (< last-ip  IP (array-dimension PROG 0))
    :finally (return  (when (< last-ip IP) (car OUT)) )
  ))
:collect a0
)

(loop
 :with PROG := #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)
  :for last-ip := -1 :then IP
  :for (A B C IP OUT) := (PROC 3 nil nil 0 nil PROG) :then (PROC A B C IP OUT PROG)
  :do (format t "A=~a B=~a C=~a IP=~a OUT=~a~%" A B C IP OUT)
  :while  (< last-ip  IP (array-dimension PROG 0))
  :finally (return (< last-ip IP))
)


(loop
 :with PROG := #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)
  :for last-ip := -1 :then IP
  :for (A B C IP OUT) := (PROC 300 nil nil 0 nil PROG) :then (PROC A B C IP OUT PROG)
  :do (format t "A=~a B=~a C=~a IP=~a OUT=~a~%" A B C IP OUT)
  :while  (< last-ip  IP (array-dimension PROG 0))
  :finally (return (< last-ip IP))
)


(
 loop
 :with PROG := #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)
 :for tgt-out :in (reverse '(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)) 
 :for acc-a := 5 :then 
  (
   loop
   :for k :from 0 :below 8
   :until
    (= tgt-out 
      (loop
        ;:with PROG := #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)
        :for last-ip := -1 :then IP
        :for (A B C IP OUT) := (PROC (+ k (ash acc-a 3)) nil nil 0 nil PROG) :then (PROC A B C IP OUT PROG)
        ;:do (format t "A=~a B=~a C=~a IP=~a OUT=~a~%" A B C IP OUT)
        :while  (< last-ip  IP (array-dimension PROG 0))
        :finally (return (car OUT))))
   :finally (progn
           (format t "A~a TGT~a~%" (+ k (ash acc-a 3)) tgt-out)
           (return (+ k (ash acc-a 3))))
)
  :finally (return acc-a)
  )

(exec-prog 190354176359439 nil nil 0 #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)) 

(exec-prog 190615597431823 nil nil 0 #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)) 

(
 loop
 :with PROG := #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)
 :for tgt-out :in (reverse '(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0)) 
 :for acc-a := '(5) :then
  (
   loop :for cnd-a :in acc-a
   :append  
   (loop
   :for k :from 0 :below 8
   :for a0 := (+ k (ash cnd-a 3))
   :for out := (exec-prog a0 nil nil 0 #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0))
   :when (= tgt-out (car out) )
   :collect a0 :into candidates
   :finally (return (progn (format t "TGT~a A~a OUT~a~%" tgt-out candidates out) candidates)))
  )
  :finally (return acc-a)
  )

   (loop
   :with acc-a := 1418248
   :for k :from 0 :below 8
   :for a0 := (+ k (ash acc-a 3))
   :do (print a0)
  )

11361574