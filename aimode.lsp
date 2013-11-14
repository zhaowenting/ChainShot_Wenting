(load "board.fas")
(load "result.fas")

(defparameter *size* 0)  ;size records the number of rows or columns of the current board
(defparameter filename "boardtest.txt")
(defparameter *nil-beads* 0)
(defparameter *move-records* nil)
(defparameter *score* 0)
(defparameter *cluster-key* nil)  ;*cluster-key* stores all the cluster-key nodes
(defparameter *h-value* 0)  ; h-value iniatialization
(defparameter clusteritems nil)
(defparameter clustermembers nil)
(defparameter heuristic-nilbeads-list nil)
(defparameter *timeelipsed* 0)
(defparameter *cluster-moves* nil)

(defun ai-mode ()
  ;;play the game in ai mode.
  (setf *timeelipsed* 0)
  (setf *nil-beads* 0)
  (setf *score* 0)
  (setf *move-records* nil)
					;board file is imported manually
  (setf result (boardimport))
  (setq filename (car result))
  (setq *size* (cadr result))
					;board inilization
  (defparameter *board* (make-array (list *size* *size*)))
  (setf file (open filename))
  (dotimes (row *size*)
    (dotimes (col *size*)
      (setf (aref *board* row col) (read-char file)))
    (read-char file nil :eof))
  (close file)  ;boarddata is imported succefully
  (boarddraw)
  (clusterrecord nil *nil-beads*)  ;record the neighbour value of each cluster key node
  (loop while (ainotover) do ;move search
       (setq timestart (get-internal-run-time))
       (setf niladd (nextmove))    ;make a move after comparing its heuristic value
       (setq timeend (get-internal-run-time))
       (incf *timeelipsed* (float (/ (- timeend timestart) internal-time-units-per-second)))
       (resultrefresh niladd)  ;refresh score
       (boarddraw)  ;print the current board
       (resulthint))  ;show the current score and number of beads left
  (resultoutput)  ;game ends and print the moves made
  (return-from ai-mode))

(defun clusterrecord (calc nilbeads &optional tempboard)
  ;;record the neighbour value of each cluster-key
					;define cluster-key node (which has two more neighbours own the same color with the current node itself) is viewed as cluster-key
					;cluster-key means there is necessarily a group around itself, the group can be potentially removed in the nextmove
  (setf *cluster-key* nil)
  (setf *h-value* 0)
  (if calc 
      (setq board tempboard)
      (setq board *board*))
  (dotimes (i *size*)
    (dotimes (j *size*) 
      (if (aref board i j)
	  (progn (setq counter 1 currentnode (aref board i j))
		 (if (and (> i 0) (equal currentnode (aref board (1- i) j)))
		     (incf counter))
		 (if (and (< j (1- *size*)) (equal currentnode (aref board i (1+ j))))
		     (incf counter))
		 (if (and (< i (1- *size*)) (equal currentnode (aref board (1+ i) j)))
		     (incf counter))
		 (if (and (> j 0) (equal currentnode (aref board i (1- j))))
		     (incf counter))
		 (setq *h-value* (+ *h-value* counter))
		 (if (>= counter 3)
		     (push (list i j) *cluster-key*))))))
  (setq *h-value* (+ *h-value* (* 5 nilbeads))))   
					; HeuristicValue=HeuristicValueofCurrentBoard + 5 * #ofBeadsLeft  -->Heuristic Function


(defun ainotover()
  ;;check if the current game can continue, or interrepted by the user before it ends
  (if (> (length *cluster-key*) 0 )
      (return-from ainotover T)
      (return-from ainotover nil)))

(defun nextmove ()
  ;;make the next move by the highest heuristic value
  (loop while (> (length *cluster-key*) 0)do
					;store the clusters, each of its member is an individual cluster group
       (setq clusteritems nil)
       (setq current-key (pop *cluster-key*))
       (setq ck-row (car current-key) ck-col (cadr current-key))
       (setq clustermembers nil)
					;  (format t "   ckrow ~A   " ck-row)    
       (ailegalmove ck-row ck-col (aref *board* ck-row ck-col)) ;read each node of *cluster-key*, make cluster list for its group
       (push clustermembers clusteritems))
					;compare different heuristic values, find the largest one refresh *board*  
  (defparameter heuristic-list nil)
  (defparameter board-list nil)
  (loop while (> (length clusteritems) 0) do
       (setq temp-board (copy-array *board*))
       (setq clusterreader (pop clusteritems))
       (setq nilcounter 0)
       (loop while (> (length clustermembers) 0) do
;traverse every cluster(items), makenil the beads of cluster(members),store the refreshed heuristic into hvalue-leftbeads, find the maximum
	    (setf makenil-index (pop clustermembers))
	    (incf nilcounter)
	    (setf (aref temp-board (car makenil-index) (cadr makenil-index)) nil))
       (push (list (1+ (car makenil-index)) (1+ (cadr makenil-index))) *cluster-moves*)
       (boardrefresh T)

       (setf nilbeads (+ *nil-beads* nilcounter))
       (setf hvalue-leftbeads (list (clusterrecord T nilbeads temp-board) nilbeads))
       (push hvalue-leftbeads heuristic-nilbeads-list);store the refreshed h-value & #of nil beads in heuristic-list (for move comparasion)
       (push temp-board board-list))

  (setq findmax-h (pop heuristic-nilbeads-list))
  (setq findmax-board (pop board-list))
  (setq findmax-move (pop *cluster-moves*))
  (dotimes (i (length heuristic-nilbeads-list))
    (setq current-h (pop heuristic-nilbeads-list))
    (if (< findmax-h (car current-h))
	(progn (setq findmax-board (pop board-list))
	       (setq findmax-move (pop *cluster-moves*)))
	(progn (pop board-list)
	       (pop *cluster-moves*))))

					;complete comparasion, store the current *board*
  (setq *board* (copy-array findmax-board))
					;refresh the nilbeads
  (setq addnil (- (cadr findmax-h) *nil-beads*))
  (setq *nil-beads* (cadr findmax-h))
  (push findmax-move *move-records*)
  (return-from nextmove addnil))


(defun ailegalmove (lr lc color)
  (if (member (list lr lc) clustermembers :test #'equal)
      ()
      (progn (push (list lr lc) clustermembers)
	     (if (member (list lr lc) *cluster-key* :test #'equal) (remove (list lr lc) *cluster-key*))
	     (if (and (> lr 0) (equal color (aref *board* (1- lr) lc)))
		 (ailegalmove (1- lr) lc color))
	     (if (and (< lr (1- *size*)) (equal color (aref *board* (1+ lr) lc)))
		 (ailegalmove (1+ lr) lc color))
	     (if (and (> lc 0) (equal color (aref *board* lr (1- lc))))
		 (ailegalmove lr (1- lc) color))
	     (if (and (< lc (1- *size*)) (equal color (aref *board* lr (1+ lc))))
		 (ailegalmove lr (1+ lc) color)))))

(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))
