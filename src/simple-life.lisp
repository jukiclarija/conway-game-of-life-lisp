;;; JEDNOSTAVNI GAME OF LIFE - START/STOP

;; Očisti ekran
(format t "~C[2J~C[H" #\Esc #\Esc)

;; Parametri
(defparameter *size* 20)
(defparameter *delay* 0.2)
(defparameter *running* t)

;; Grid funkcije
(defun make-grid ()
  (make-array (list *size* *size*) :initial-element 0))

(defun random-grid ()
  (let ((grid (make-grid)))
    (dotimes (i *size* grid)
      (dotimes (j *size*)
        (setf (aref grid i j) (if (< (random 1.0) 0.25) 1 0))))))

;; Susjedi s wrap-around (da ne stane)
(defun count-neighbors (grid r c)
  (let ((cnt 0))
    (loop for di from -1 to 1 do
      (loop for dj from -1 to 1 do
        (unless (and (= di 0) (= dj 0))
          (let ((ni (mod (+ r di) *size*))
                (nj (mod (+ c dj) *size*)))
            (when (= (aref grid ni nj) 1)
              (incf cnt))))))
    cnt))

(defun next-generation (grid)
  (let ((new (make-grid)))
    (dotimes (i *size* new)
      (dotimes (j *size*)
        (let ((cell (aref grid i j))
              (n (count-neighbors grid i j)))
          (setf (aref new i j)
                (cond ((and (= cell 1) (< n 2)) 0)
                      ((and (= cell 1) (<= 2 n 3)) 1)
                      ((and (= cell 1) (> n 3)) 0)
                      ((and (= cell 0) (= n 3)) 1)
                      (t cell))))))))

;; Nacrtaj grid
(defun draw-all (grid gen)
  ;; Pomakni na vrh
  (format t "~C[H" #\Esc)
  
  ;; Naslov
  (format t "~C[1;36m" #\Esc)
  (format t "GAME OF LIFE - 20x20~%~%")
  (format t "~C[0m" #\Esc)
  
  ;; Grid s okvirom
  (format t "~C[1;34m┌" #\Esc)
  (dotimes (i 40) (format t "─"))
  (format t "┐~C[0m~%" #\Esc)
  
  (dotimes (i *size*)
    (format t "~C[1;34m│~C[0m" #\Esc #\Esc)
    (dotimes (j *size*)
      (if (= (aref grid i j) 1)
          (format t "██")
          (format t "  ")))
    (format t "~C[1;34m│~C[0m~%" #\Esc #\Esc))
  
  (format t "~C[1;34m└" #\Esc)
  (dotimes (i 40) (format t "─"))
  (format t "┘~C[0m~%" #\Esc)
  
  ;; Info
  (format t "~%Generation: ~d~%" gen)
  (format t "Press Ctrl+C to stop~%"))

;; Glavna petlja
(defun run-simple ()
  (let ((grid (random-grid))
        (gen 0))
    
    ;; Sakrij kursor
    (format t "~C[?25l" #\Esc)
    
    ;; Petlja
    (handler-case
        (loop while *running* do
          (draw-all grid gen)
          (setf grid (next-generation grid))
          (incf gen)
          (sleep *delay*))
      
      (t ()
         ;; Vrati kursor
         (format t "~C[?25h" #\Esc)
         (format t "~%Stopped at generation ~d~%" gen)))))

;; Start
(format t "Starting Game of Life...~%")
(sleep 1)
(run-simple)
