(defun boardimport()
;;manually import a board by reading a file
  (setq itemnum 0)
  (dolist (directoryitem (directory "*"))
    (if (equal (pathname-type directoryitem) "txt")
	(format t "data~A: ~A.~A ~%" (incf itemnum) (pathname-name directoryitem)(pathname-type directoryitem))))
  (princ "Please input goal board by its name (e.g. test1.txt):")
  (loop while (not (probe-file (setq filename (read-line)))) do
    (princ "Please input valid boardname:"))
  (setf file (open filename))
  (setq size (length (read-line file)))
  (close file)
;get the size of the board
  (return-from boardimport (list filename size)))

(defun boarddraw ()
;;draw a board according the selected board
  (format t "~%   |")
  (dotimes (colindex *size*)
    (if (< colindex 9)
	(format t " ~A |" (+ 1 colindex))
	(format t " ~A|" (+ 1 colindex))))
;uppermost hyphen output
  (format t "~% - |")
  (dotimes (hyphen (- *size* 1)) (format t "----"))
  (format t "---|~%")
;beads output
  (dotimes (row *size*)
    (if (< row (- *size* 1 ))
	(progn (if (< row 9)
		   (format t " ~A |" (+ 1 row))
		   (format t " ~A|" (+ 1 row)))
	       (dotimes (column *size*)
                    (if (aref *board* row column)
                        (format t " ~A |" (aref *board* row column))
                        (format t "   |")))
	       (format t "~% - |")
	       (dotimes (hyphen-plus (- *size* 1)) (format t "---+"))
	       (format t "---|~%"))
	(progn (if (< row 9)
		   (format t " ~A |" (+ 1 row))
		   (format t " ~A|" (+ 1 row)))
	       (dotimes (column *size*)
                    (if (aref *board* row column)
                        (format t " ~A |" (aref *board* row column))
                        (format t "   |"))))))
;bottom hyphen output
  (format t "~% - |")
  (dotimes (hyphen (- *size* 1)) (format t "----"))
  (format t "---|~%"))


(defun boardrefresh(calc)
;;refresh board after a move with vertical and horizontal merge
;vertical merge
  (if calc 
      (setf board temp-board)
      (setf board *board*))
  (dotimes (j *size*)
    (let* ((i (1- *size*)) (k (1- i)))
      (loop while (and (> i 0) (>= k 0)) do
	   (if (aref board i j) (progn (decf i) (setf k (1- i)))
	       (progn (loop while (and (>= k 0) (not (aref board k j))) do (decf k)) 
		      (if (>= k 0) (progn (setf (aref board i j) (aref board k j))
					  (setf (aref board k j) nil))))))))
;horizontal merge
  (let* ((j 0) (k (1+ j)))
    (loop while (and (< j (1- *size*)) (< k *size*)) do
	 (if (aref board (1- *size*) j) (progn (incf j) (setf k (1+ j)))
	     (progn (loop while (and (< k *size*) (not (aref board (1- *size*) k))) do (incf k))
		    (if (< k *size*)
			(progn (dotimes (m *size*)
				 (setf (aref board m j) (aref board m k))
				 (setf (aref board m k) nil)))))))))
