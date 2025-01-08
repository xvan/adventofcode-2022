(ql:quickload "Alexandria")

(defun str-to-list (str)
(loop :for c :across str :collect c))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun load-map (file)
  (with-open-file (in file)
  
     (list-to-2d-array (loop          
                  :for riv = (read-line in nil)                   
                  :while riv
                  :collect (str-to-list riv) ))))

(defmacro aref-2d (array indices)
  `(aref ,array (first ,indices) (second ,indices)))

(defun validate-coord (coord mapa)
  (and 
   (every #'identity (mapcar (lambda (x y) (and (>= x 0) (< x y))) coord (array-dimensions mapa)))
   (not (eq (aref-2d mapa coord) #\#))
   ))

(defun get-neighbours (c mapa)
  (remove-if-not (lambda (n) (validate-coord n mapa)) (mapcar (lambda (d) (mapcar #'+ c d)) '((0 1) (-1 0) (0 -1) (1 0) ))))


(defun find-start (mapa)
  (loop
    :named outer  
    :with rows := (array-dimension mapa 0)
    :with cols := (array-dimension mapa 1)    
    :for row :from 0 :below rows
    :do (loop
      :for col :from 0 :below cols
      :for coord := (list row col)
      :for tile := (aref-2d mapa coord)
      :when (eq tile #\S)
      :do (return-from outer coord)
      )
    )
)

(defun step-mapa (mapa prev-coord current-coord)
  (when current-coord
    (cons 
          current-coord
          (step-mapa mapa current-coord (car (remove-if (lambda (x) (equalp x prev-coord)) (get-neighbours current-coord mapa)))))
  )
)

(defun find-exit (mapa)
  (step-mapa mapa nil (find-start mapa))
  )

(defun distance (c1 c2)
  (apply #'+ (mapcar #'abs (mapcar '- c1 c2)))
  )

(defun count-shortcuts (shortcuts)
    (let ((all-entries nil))
 (loop 
 :with catbag := (make-hash-table)
 :for (shortcut delta) :in shortcuts
 :do (incf (gethash delta catbag 0))
 :finally (maphash (lambda (k v) (push (list k v) all-entries)) catbag)
 )
 (sort all-entries #'< :key #'first)
  ))

(defun solve-first (file limit)
 (let* (
         (mapa (load-map file))
         (path (coerce (reverse (find-exit mapa)) 'vector))
         )
    
    (apply #'+
     (mapcar #'second (remove-if (lambda (x) (< (first x) limit))
     (count-shortcuts
      (loop
      :with catbag := (make-hash-table :test #'equalp)
      :with base-length :=  (length path)
      :for jump-to :from 0 :below (- base-length 3)
      :for jump-to-coord := (aref path jump-to)
      :append (loop 
                :for jump-from :from (+ jump-to 3) :below base-length
                :for jump-from-coord := (aref path jump-from)        
                :when (= 2 (distance jump-from-coord jump-to-coord))
                :collect (list (list jump-from-coord jump-to-coord) (- jump-from jump-to 2))
          )
      )
    ))))
    ))


(defun solve-second (file limit)
 (let* (
         (mapa (load-map file))
         (path (coerce (reverse (find-exit mapa)) 'vector))
         )
    
    (apply #'+
     (mapcar #'second (remove-if (lambda (x) (< (first x) limit))
     (count-shortcuts
      (loop :for cheat-size :from 2 :to 20                                  
      :append (loop
      :with catbag := (make-hash-table :test #'equalp)
      :with base-length :=  (length path)
      :for jump-to :from 0 :below (- base-length (1+ cheat-size))
      :for jump-to-coord := (aref path jump-to)      
      :append (loop 
                :for jump-from :from (+ jump-to (1+ cheat-size)) :below base-length
                :for jump-from-coord := (aref path jump-from)        
                :when (= cheat-size (distance jump-from-coord jump-to-coord))
                :collect (list (list jump-from-coord jump-to-coord) (- jump-from jump-to cheat-size))
          )
      ))
    ))))
    ))

(solve-first "2024/day20/test_input0" 0)

(solve-first "2024/day20/input" 100)
(solve-second "2024/day20/input" 100)

