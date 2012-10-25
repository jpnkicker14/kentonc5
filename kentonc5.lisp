; Wizards_game from Land of Lisp
; Assignment 5
; Author: Kenton Chun
; Author: Amy Takayesu
; a lot of code comes from the Land of Lisp book

(defparameter *ID* "Kenton Chun & Amy Takayesu")


;allowed commands
(defparameter *allowed-commands* '(look walk pickup inventory help))

;nodes for the scenery stored in a list
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))
                        (bar (you are in a refurnished bar.
                            there is a counter to your left.))))

;edges will store the paths and the choices you have from that location
(defparameter *edges* '((living-room (garden west door)  
                                     (attic upstairs ladder)
                                          (bar south revolving-door))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))
                        (bar (living-room north revolving-door))))

;objects on the ground parmater 
(defparameter *objects* '(whiskey bucket frog chain coaster glass vodka tonic-water lime))

; the list of where the objects are located
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)
                                   (coaster bar)
                                   (glass attic)
                                   (vodka bar)
                                   (tonic-water living-room)
                                   (lime garden)))

;the default location 
(defparameter *location* 'living-room)

;keeps track if chain has been welded to the bucket
(defparameter *chain-welded* nil)

;keeps track if bucket is filled
(defparameter *bucket-filled* nil)

;keeps track if glass has vodka in it
(defparameter *vodka-in-glass* nil)

;keeps track tonic-water has been added to the vodka in the glass
(defparameter *water-in-drink* nil)

;keeps track of if the complete vodka tonic drink has been made
(defparameter *complete-vodka-tonic* nil)

;function that describes the location
;this function will use the assoc to find the location based from the nodes
;then cadr will return the description  
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;returns the descripiton based on the edge
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;will call the describe path if there is more than one possible path
;the map car will be applyed to every member of the list  
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;the function that will return the objects at a given location
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;function to describe the objects visible at a given location
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;will return all of the parameters
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;takes a direction and moves to that location if it's legal
(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

;tracks the location of objects 
(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
          (t '(you cannot get that.))))

;keeps track of the objects that were picked up 
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;used to check if you have an object
(defun have (object) 
    (member object (cdr (inventory))))

;starts the special game REPL
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

;reads the commands 
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;calls the commands
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;alters the output for game-print
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

;prints the result in the custom REPL
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))

;prints out descriptions of available commands
(defun help ()
(princ "HELP:")(terpri)
  (princ "look: Describes your current location. This includes objects in the room, furniture, and exits.")(terpri)
  (princ "walk direction: Choose which direction to walk in, and your character will do so, as long as there is some kind of an exit in that direction.")(terpri)
  (princ "pickup item: Pick up an item and put it in your inventory.")(terpri)
  (princ "have item: Checks if you have an item in your possession.")(terpri)
  (princ "inventory: Lists all of the items that you currently have in your possession."))


(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

(game-action pour vodka glass bar
             (if (and (have 'glass) (not *vodka-in-glass*))
                 (progn (setf *vodka-in-glass* 't)
                        '(the glass now has two ounces of vodka in it.))
               '(you do not have a glass.)))

(game-action add tonic-water glass bar
             (if *vodka-in-glass*
                 (progn (setf *water-in-drink* 't)
                        '(the glass has been filled to the top with tonic-water.))
               '(you must add vodka before filling the glass with tonic-water.)))

(game-action garnish lime glass bar
             (if *water-in-drink* 
                 (progn (setf *complete-vodka-tonic* 't)
                        '(the lime has been artistically placed on the rim of the glass.))
               '(you must finish mixing your drink before garnishing it.)))

(game-action offer glass wizard living-room
             (cond ((not *complete-vodka-tonic*) '(the drink has not been completely prepared.))
                   ((have 'coaster) '(the wizard awakens and gladly accepts his favorite drink-a vodka tonic.
                                      a disco ball is lowered from the ceiling. music starts playing.
                                      the party has started! congratulations! you win.))
                   (t '(the wizard awakens and gladly accepts his favorite drink-a vodka tonic.
                        however once he places it on the coffee table it leaves a ring because you 
                        forgot to give him a coaster. shame on you! you lose.))))


(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

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

(defmacro add-object (object location &body body)
  `(progn (if(and(not(member ',object *objects* :test 'equal))
	      (member ',location (mapcar #'car *nodes*)))
	      ,@body
	      (progn
		(pushnew ',object *objects*)
		(pushnew '(,object ,location) *object-locations*) '(sucessfully added object.))
	      '(try again. the object already exists or the location does not exist.)) 
	  )
)

(defmacro add-location (location &rest desc)
  `(progn (if(not(member ',location (mapcar #'car *nodes*))) 
     (progn(pushnew '(,location (,@desc)) *nodes*)
     '(SUCCESS))
     '(FAIL)))
)