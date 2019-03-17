# locations
(setq *nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
                (garden (you are in a beautiful garden. there is a well in front of you.))
                (attic (you are in the attic. there is a giant welding torch in the corner.))))

# get description of a location
(de describe-location (location nodes)
   (cadr (assoc location nodes)))

# connections between locations
(setq *edges* '((living-room (garden west door) (attic upstairs ladder))
                (garden (living-room east door))
                (attic (living-room downstairs ladder))))

# get description of connection
(de describe-path (edge)
  (let (@X (caddr edge)
        @Y (cadr edge))
    (fill '(there is a @X going @Y from here.) '(@X @Y))))

# get description of all connections
(de describe-paths (location edges)
  (apply 'append (mapcar 'describe-path (cdr (assoc location edges)))))

# objects
(setq *objects* '(whiskey bucket frog chain))

# location of objects
(setq *object-locations* '((whiskey living-room)
                           (bucket living-room)
                           (chain garden)
                           (frog garden)))

# get objects at location
(de objects-at (loc objs obj-loc)
  (let (is-at '((Obj) (== (cadr (assoc Obj obj-loc)) loc)))
     (filter 'is-at objs)))

# describe objects
(de describe-objects (loc objs obj-loc)
  (let (describe-obj '((Obj) (fill '(you see a Obj on the floor.) 'Obj)))
     (apply 'append (mapcar 'describe-obj (objects-at loc objs obj-loc)))))

# current location
(setq *location* 'living-room)

# 'look' action
(de look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

# 'walk' action
(de walk (direction)
  (let (correct-way '((Edge) (== (cadr Edge) direction))
        Next (find 'correct-way (cdr (assoc *location* *edges*))))
     (if Next 
         (prog (setq *location* (car Next)) 
               (look))
         '(you cannot go that way.))))

# 'pickup' action
(de pickup (Obj)
  (cond
     ((member Obj (objects-at *location* *objects* *object-locations*))
     (push '*object-locations* (list Obj 'body))
     (fill '(you are now carrying the Obj) 'Obj))
     (T '(you cannot get that.))))

# 'inventory' action
(de inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

# check if user has object
(de have (Obj) 
    (member Obj (cdr (inventory))))

# game-read function for the custom game-repl
(de game-read ()
  (let (cmd (in NIL (str (pack (line))))
        quote-it '((x) (cons 'quote x)))
     (cons (car cmd) (mapcar 'quote-it (cdr cmd)))))

# allowed commands
(setq *allowed-commands* '(look walk pickup inventory))

# game-eval function for the custom game-repl
(de game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

# text tweaking function
(de tweak-text (lst caps lit)
  (when lst
    (let (item (car lst)
          rest (cdr lst))
       (cond
          ((= item " ") (cons item (tweak-text rest caps lit)))
          ((member item '("!" "?" ".")) (cons item (tweak-text rest T lit)))
          ((= item "\"") (tweak-text rest caps (not lit)))
          (lit (cons item (tweak-text rest NIL lit)))
          (caps (cons (uppc item) (tweak-text rest NIL lit)))
          (T (cons (lowc item) (tweak-text rest NIL NIL)))))))

# game-print function for the custom game-repl
(de game-print (lst)
  (prinl (pack (tweak-text (chop (str lst)) T NIL))))

# custom game-repl
(de game-repl ()
  (let (cmd (game-read))
     (unless (== (car cmd) 'quit)
        (game-print (game-eval cmd))
        (game-repl))))

# generic game action
# (since picolisp has no macros, this becomes a normal function)
(de game-action (cmd subj obj place . body)
  (prog
    (set cmd (fill '((subject object)
                       (if (and (= *location* 'place)
                                (= subject 'subj)
                                (= object 'obj)
                                (have 'subj))
                           ^ body
                           '(i cant cmd like that.)))
                   '(cmd subj obj place body)))
    (push1 '*allowed-commands* cmd)))

# define 'weld' action
(setq *chain-welded* NIL)
(game-action 'weld 'chain 'bucket 'attic
   (if (and (have 'bucket)
            (not *chain-welded*))
       (prog (setq *chain-welded* T)
             '(the chain is now securely welded to the bucket.))
       '(you do not have a bucket.)))

# define 'dunk' action
(setq *bucket-filled* NIL)
(game-action 'dunk 'bucket 'well 'garden
   (if *chain-welded*
       (prog (setq *bucket-filled* 'T)
             '(the bucket is now full of water.))
       '(the water level is too low to reach.)))

# define 'splash' action
(game-action 'splash 'bucket 'wizard 'living-room
   (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
         ((have 'frog) '(the wizard awakens and sees that you stole his frog.
                         he is so upset he banishes you to the
                         netherworlds- you lose! the end.))
         (T '(the wizard awakens from his slumber and greets you warmly.
              he hands you the magic low-carb donut- you win! the end.))))
# start
(game-repl)
