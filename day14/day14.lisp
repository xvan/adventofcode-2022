
(defun split-by-string (separator string)    
    (loop with l = (length separator)
          for i = 0 then (+ j l)
          as j = (search separator string :start2 i)          
          collect (subseq string i j)
          while j))

(defun parse-line (line)
        (mapcar (lambda (pair) (mapcar 'parse-integer (split-by-string "," pair))) (split-by-string " -> " line )))
            

(defun parse-lines (lines) (mapcar 'parse-line lines))


(defvar *test-data* '("498,4 -> 498,6 -> 496,6" "503,4 -> 502,4 -> 502,9 -> 494,9"))

(ql:quickload "fiveam")

(fiveam:def-suite 14am-suite)
(fiveam:in-suite 14am-suite)

(fiveam:test parsing
    (fiveam:is (equal '(((498 4) (498 6) (496 6)) ((503 4) (502 4) (502 9) (494 9))) (parse-lines *test-data*))) 
    )


(fiveam:test test-process
    ;(fiveam:is-true  nil)    
    )

(fiveam:run! '14am-suite)