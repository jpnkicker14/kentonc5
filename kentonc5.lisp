; Wizards_game part 1

(defparameter *ID* "Kenton Chun")

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))

;nodes for the scenery stored in a list
(defparameter *nodes* '((living-room (you are in the living-room.
                            A Wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))
			(well (you are in the well.
			    there is a troll next to you.))	   
			))

;function that describes the location
;this fucntion will use the assoc to find the location based from the nodes
;then cadr will return the description  
(defun describe-location (location nodes)
  "describes the location based on a node"
   (cadr (assoc location nodes)))

;edges will store the paths and the choices you have from that location
(defparameter *edges* '((living-room (garden west door)  
                                     (attic upstairs ladder))
                        (garden (living-room east door)
				(well down rope))
                        (attic (living-room downstairs ladder))
			(well (garden up rope))
			))

;returns the descripiton based on the edge
(defun describe-path (edge)
  "describes the path based on the edge"
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;will call the describe path if there is more than one possible path
;the map car will be applyed to every member of the list  
(defun describe-paths (location edges)
  "calls the describe path passing after finding the edges knowing the location"
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;objects on the ground parmater 
(defparameter *objects* '(whiskey fabric bucket frog chain coin strap handle pockets))

; the list of where the objects are located
(defparameter *object-locations* '((whiskey living-room)
				   (fabric living-room)
				   (bucket living-room)
                                   (chain garden)
				   (strap garden)
                                   (frog garden)
				   (coin well)
				   (handle well)
				   (pockets attic)))

;the function that will return the objects at a given location   
(defun objects-at (loc objs obj-loc)
  "return the objects at a location given the list of objects and object locations lists"
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;function to describe the objects visible at a given location
(defun describe-objects (loc objs obj-loc)
  "returns the objects at a locaiton" 
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;the default location 
(defparameter *location* 'living-room)

;will return all of the paramters
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;takes a direction and moves to that location if it's legal
(defun walk (direction)
  "moves the location after moving to a direciton"
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

;tracks the location of objects 
(defun pickup (object)
  "picks up the object and will keep track of it"
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

;keeps track of the objects that were picked up 
(defun inventory ()
  "displays the inventory of objects picked up"
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;used to check if you have an object
(defun have (object) 
  "returns if you have the object" 
    (member object (cdr (inventory))))
    (defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
             (if *chain-welded* 
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water))
               '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. 
                                   he is so upset he banishes you to the 
                                   netherworlds- you lose! the end.))
                   (t '(the wizard awakens from his slumber and greets you warmly. 
                        he hands you the magic low-carb donut- you win! the end.))))
