(ns land-of-lisp.evolution.core)

(def gene-num "number of genes/directions" 8)
(def plant-energy "energy of each plant" 80)
(def reprod-energy "energy required for reproduction" 200)

(defrecord Area [left top width height])
(defrecord Animal [x y dir energy genes type])
(defrecord Plant [x y energy type])

(def world "world area" (Area. 0 0 100 30))
(def jungle "jungle area" (Area. 45 10 10 10))

(defn location-check
  [obj1 obj2]
  (and (= (:x obj1) (:x obj2)) (= (:y obj1) (:y obj2))))

(defn animal?
  [actor]
  (= (:type actor) :animal))

(defn plant?
  [actor]
  (= (:type actor) :plant))

(defn dead?
  [actor]
  (<= (:energy actor) 0))

(defn random-plant
  [area]
  (Plant. (+ (:left area) (inc (rand-int (:width area))))
          (+ (:top area) (inc (rand-int (:height area))))
          plant-energy
          :plant))

(defn add-plants
  [actors]
  (conj actors (random-plant jungle) (random-plant world)))

(defn angle
  [genes]
  (loop [g genes
         x (rand-int (apply + genes))
         a 0]
    (let [x-new (- x (first g))]
      (if (< x-new 0)
        a
        (recur (rest g) x-new (inc a))))))

(defn turn
  [dir genes]
  (mod (+ dir (angle genes)) gene-num))

(defn move-x
  [x dir]
  (mod (+ x (cond
             (some #(= % dir) [2 3 4]) 1
             (some #(= % dir) [6 7 0]) -1
             :else 0))
       (:width world)))

(defn move-y
  [y dir]
  (mod (+ y (cond
             (some #(= % dir) [4 5 6]) 1
             (some #(= % dir) [0 1 2]) -1
             :else 0))
       (:height world)))

;; 0 1 2
;; 7 A 3
;; 6 5 4
(defn move
  [actor]
  (if (animal? actor)
    (let [x (:x actor)
          y (:y actor)
          e (:energy actor)
          d (:dir actor)
          g (:genes actor)]
      (Animal. (move-x x d)
               (move-y y d)
               (turn d g)
               (- e 1)
               g
               :animal))
    actor))

(defn move-all
  [actors]
  (map move actors))

(defn eat-at-cell
  [group]
  (let [a (filter animal? group)
        na (count a)
        p (filter plant? group)
        np (count p)]
    #_(println a p)
    #_(println na np)
    (if (and (not-empty a) (not-empty p))
      (cond
       (>= na np) (map-indexed (fn [i o]
                                 (if (< i na)
                                   (update-in o [:energy] + plant-energy)
                                   o))
                               a)
       :else (flatten (cons (map #(update-in % [:energy] + plant-energy) a)
                            (drop na p))))
      group)))

(defn eat-all
  [actors]
  (let [group (vals (group-by (fn [a] [(:x a) (:y a)]) actors))]
    #_(println group)
    #_(println (flatten (map eat-at-cell group)))
    (flatten (map eat-at-cell group))))

(defn mutate
  [genes]
  (let [n-mut (rand-int gene-num)
        mutation (max 1 (+ (nth genes n-mut) (rand-int 3) -1))]
    (assoc genes n-mut mutation)))

(defn reprod
  [actor]
  (if (animal? actor)
    (let [e (:energy actor)
          g (:genes actor)]
      (if (> e reprod-energy)
        (let [e-new (quot e 2)
              g-new (mutate g)
              parent (assoc actor :energy e-new)]
          (list parent (assoc parent :genes g-new)))
        (list actor)))
    actor))

(defn reprod-all
  [actors]
  #_(println actors)
  #_(println (flatten (map reprod actors)))
  (flatten (map reprod actors)))

(defn remove-dead-actors
  [actors]
  (remove dead? actors))

(defn update-world
  [actors n]
  (let [animals-upd (remove-dead-actors actors)
        plants-upd (add-plants animals-upd)
        actors-new ((comp reprod-all eat-all move-all) plants-upd)]
    #_(println actors)
    #_(println animals-upd)
    #_(println plants-upd)
    #_(println actors-new)
    (if (> n 1)
      (do
        #_(println "turn" n)
        (recur actors-new (dec n)))
      actors-new)))

(defn init-plants
  []
  ())

(defn init-animals
  []
  (list (Animal. (quot (:width world) 2)
                 (quot (:height world) 2)
                 0
                 1000
                 (into [] (for [x (range gene-num)] (inc (rand-int 10))))
                 :animal)))

(defn h-line
  []
  (flush)
  (print "+")
  (dotimes [x (:width world)] (print "-"))
  (print "+"))

(defn draw-world
  [actors]
  (let [animals (filter animal? actors)
        plants (filter plant? actors)]
    (newline)
    (h-line)
    (dotimes [y (:height world)]
      (flush)
      (print "|")
      (dotimes [x (:width world)]
        (let [check (partial location-check {:x x :y y})]
          (cond
           (some check animals) (print "A")
           (some check plants) (print "*")
           :else (print " "))))
      (print "|"))
    (h-line)
    (flush)))

(defn pick-turns
  []
  "Pick a number of turns"
  (println "Pick a turn number #: ")
  (let [n (read-line)]
    (if (re-matches #"[0-9]+" n)
      (let [x (read-string n)]
        (if (>= x 1)
          x
          (recur)))
      (recur))))

(defn evolution
  "Run the game"
  []
  (println "Welcome to EVOLUTION!")
  (let [n (pick-turns)
        new-world (update-world (init-animals) n)]
    (draw-world new-world)
    new-world))

;;(time (evolution))



;; (def animals (list (Animal. 1 2 450 4 [1 2 3 4 5 6 7 8] :animal) (Animal. 3 4 0 10 '(1 2 3 4 5 6 7 8)) (Animal. 5 6 0 10 '(1 2 3 4 5 6 7 8))))
;; (def plants (list (Plant. 1 2 10 :plant) (Plant. 3 4 10) (Plant. 7 8 10)))
;; (reprod-all (eat-all (move-all {:as animals :ps plants})))
;; (remove dead? (init-animals))
