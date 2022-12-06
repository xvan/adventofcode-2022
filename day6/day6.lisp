(ql:quickload "fiveam")



(defun find-marker (s n)
    (loop for i = n then (+ i p 1)
          as marker = (subseq s (- i n) (1- i))
          as c = (char s (1- i))
          as p = (position c marker)
          do (format t "~a~%" (list marker c p))       
          when (not p) do (return i)))


(defun find-marker-2 (s n)
    (loop for i from n to (length s)
          as marker = (subseq s (- i n) i)
          when (not (find-duplicates (to-charlist marker))) return i))

(defun to-charlist (s) (coerce s 'list))

(defun find-duplicates (s)
    (loop for c = (car s) then (car d)
          for d = (cdr s) then (cdr d)          
          while d
          when (member c d) do (return c)))

(defun read-input (file f)
  (with-open-file (in file)    
    (loop         
        :for riv = (read-line in nil)
        :while riv
        :as r = (funcall f riv)
        :when r do (return r))))

(fiveam:test test-find-marker 
    (let ((n 4))
     ;(fiveam:is  (equal 5  (find-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" n)))
     ;(fiveam:is  (equal 6  (find-marker "nppdvjthqldpwncqszvftbrmjlhg" n)))
     (fiveam:is  (equal 10 (find-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" n)))
     (fiveam:is  (equal 11 (find-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" n)))))

;(fiveam:test test-input-first (fiveam:is  (equal 1155 (read-input "day6/input" (lambda (b) (find-marker b 4))))))

;(fiveam:test test-find-marker-second    
;     (fiveam:is  (equal 19 (find-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)))
;     (fiveam:is  (equal 23 (find-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 14)))
;     (fiveam:is  (equal 23 (find-marker "nppdvjthqldpwncqszvftbrmjlhg" 14)))
;     (fiveam:is  (equal 29 (find-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14)))
;     (fiveam:is  (equal 26 (find-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14))))

;(fiveam:test test-input-first (fiveam:is  (equal 2789 (read-input "day6/input" (lambda (b) (find-marker b 14))))))

(fiveam:run!)
