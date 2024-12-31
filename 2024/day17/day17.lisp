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

( defun reverse-execution (PROG)
 (let ((acc-a '(0)))(
 loop
 :for tgt-out :across (reverse PROG) 
 :do (setq acc-a 
  (
   loop :for cnd-a :in acc-a
   :append  
   (loop
   :for k :from 0 :below 8
   :for a0 := (+ k (ash cnd-a 3))
   :for out := (exec-prog a0 nil nil 0 PROG)
   :when (= tgt-out (car out) )
   :collect a0 :into candidates
   :finally (return (progn 
                           ;(format t "TGT~a A~a OUT~a~%" tgt-out candidates out) 
                           candidates)))
  ))
  :finally (return acc-a)
  )))


(defun solve-second (file)
  (car (reverse-execution (car (reverse (read-input file)))))
  )

(reverse-execution #(2 4 1 2 7 5 0 3 1 7 4 1 5 5 3 0))

(solve-first "2024/day17/test_input0")
(solve-first "2024/day17/test_input1")
(solve-first "2024/day17/input")

(solve-second "2024/day17/test_input1")
(time (solve-second "2024/day17/input"))