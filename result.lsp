(defun resultrefresh(niladd)
;;keep record the number of beads, current score, moves
  (incf *score* (* (- niladd 2) (- niladd 2))))

(defun resulthint()
;;when a game ends, output the number of beads left and score, moves
  (format t "Current Score:~A || Beads Left:~A~%" *score* (- (* *size* *size*) *nil-beads*)))

(defun resultoutput()
;;output the total time for each move search
  (format t "~%")
  (dotimes (i 100)
    (format t "*"))
  (format t "~%Game is over!!~%Final Score:~A || Beads Left:~A" *score* (- (* *size* *size*) *nil-beads*))
  (format t "~%Move Steps: ")
  (setf *move-records* (reverse *move-records*))
  (loop while (> (length *move-records*) 0) do
       (format t "~A" (pop *move-records*)))
  (format t "~%time used is: ~As" *timeelipsed*))
