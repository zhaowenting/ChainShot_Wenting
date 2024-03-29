(load "humanmode.fas")
(load "aimode.fas")

(defun chainshot ()
;Rule Output
  (dotimes (i 100) 
    (format t "~A" "*"))
  (format t "~%Welcome to ChainShot Game!~%") 
  (dotimes (i 100) 
    (format t "~A" "*"))
  (format t "~%How to play:~%")
  (print "1. The goal is to clear all the beads")
  (print "2. You may only clear 3 or more beads at a time")
  (print "3. Only beads that are immediately adjacent to each other may be cleared")
  (print "4. Make a move by inputing the horizontal and vertical coordinators")
  (format t "~%")
  (dotimes (i 100) 
    (format t "~A" "*"))
;Select AI Mode or Human Mode
  (print "Please select AI mode or Human mode: [a/h]")
  (setf mode (read))
  (cond ((eql mode 'a) (ai-mode))
	((eql mode 'h) (human-mode))
	(t (print "Illegan Choice!") (chainshot)))
;Game is over, check if to play again
  (print "Play Again? [y/n]")
  (setf moregame (read))
  (cond ((eql moregame 'y) (chainshot))
	(t (progn (print "Bye!")
		  (return-from chainshot T)))))
