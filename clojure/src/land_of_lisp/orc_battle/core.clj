(ns land-of-lisp.orc-battle.core
  (:gen-class))

(defn randval
  "Random integer value between 1 and n" 
  [n]
  (inc (rand-int (max 1 n))))

(def monster-num "Number of opponents" 12)

(defprotocol actions
  "Actor actions"
  (show   [actor]        "Show actor stats to player")
  (attack [actor target] "Actor attacks target; returns updated actor and target")
  (hit    [actor damage] "Actor is hit for damage; returns updated actor"))

(defn dead?
  "Check if an actor is dead"
  [actor]
  (<= (:health actor) 0))

(defn monsters-dead?
  "Check if all monsters are dead"
  [monsters]
  (every? dead? monsters))

(defn player-attack
  "Recursive attack function for the player"
  [actors pick-fun d n]
  (let [p (:p actors)
        ms (:ms actors)
        i (pick-fun ms)
        m (ms i)
        p-new p
        ms-new (assoc ms i (hit m d))
        outcome {:p p-new :ms ms-new}]
    (if (or (monsters-dead? ms-new)
            (<= n 1))
      outcome
      (recur outcome pick-fun d (dec n)))))

(defn pick-monster
  "Let the player pick a monster"
  [monsters]
  (println "Pick a monster #: ")
  (let [n (read-line)]
    (if (re-matches #"[0-9]+" n)
      (let [x (read-string n)]
        (cond
         (and (>= x 1) (<= x monster-num)) (dec x)
         :else (recur monsters)))
      (recur monsters))))

(defn random-monster
  "Pick a random monster from the monsters' list"
  [ms]
  (let [n (rand-int (count ms))]
    (if (dead? (ms n))
      (recur ms)
      (do
        (println "Random monster #:" (inc n))
        n))))

(defn show-monsters
  "Show all the monsters to the player"
  [ms]
  (println "Your foes:")
  (dotimes [x (count ms)]
    (let [m (ms x)]
      (println (str "   " (inc x) ".")
               (if (dead? m)
                 "**dead**"
                 (str "(Health=" (:health m) ") " (with-out-str (show m))))))))

(defrecord Player [health agility strength])
(extend-type Player
  actions
  (show [p]
    (println (str "You are a valiant knight with a health of " (:health p)
                  ", an agility of " (:agility p)
                  " and a strength of " (:strength p))))
  (attack [p ms]
    (show-monsters ms)
    (println "Choose an attack style: [s]tab, [d]ouble swing or [r]oundhouse swing")
    (let [s (:strength p)
          a {:p p :ms ms}]
      (case (read-line)
        "s" (let [d (+ 2 (randval (quot s 2)))]
              (player-attack a pick-monster d 1))
        "d" (let [d (randval (quot s 6))]
              (println "Your double swing has stength of" d)
              (player-attack a pick-monster d 2))
        "r" (let [d 1
                  n (inc (randval (quot s 3)))]
              (player-attack a random-monster d n))
        (recur p ms))))
  (hit [p d]
    (let [p-hit (update-in p [:health] - d)]
      (if (dead? p-hit)
        (println "You have been killed.")
        (println "You have been hit for" d "hit points of damage"))
      p-hit)))
(defn make-player "Creates a new player" [] (Player. 30 30 30))

(defrecord Orc [health level])
(extend-type Orc
  actions
  (show [m]
    (print "A wicked orc with a level" (:level m) "club"))
  (attack [m p]
    (let [d (randval (:level m))]
      (println "An orc swings his club at you")
      {:m m :p (hit p d)}))
  (hit [m d]
    (let [m-hit (update-in m [:health] - d)]
      (if (dead? m-hit)
        (println "You killed the orc!")
        (println "You hit the orc, knocking off" d "health point(s)!"))
      m-hit)))
(defn make-orc "Creates a new orc" [] (Orc. (randval 10) (randval 8)))

(defrecord Hydra [health])
(extend-type Hydra
  actions
  (show [m]
    (print "A malicious hydra with" (:health m) "heads"))
  (attack [m p]
    (let [d (randval (quot (:health m) 2))]
      (println "A hydra attacks you with" d "of his heads! It also grows back one more head")
      {:m (update-in m [:health] inc) :p (hit p d)}))
  (hit [m d]
    (let [m-hit (update-in m [:health] - d)]
      (if (dead? m-hit)
        (println "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
        (println "You loop off" d "of the hydra's heads!"))
      m-hit)))
(defn make-hydra "Creates a new hydra" [] (Hydra. (randval 10)))

(defrecord Slime [health sliminess])
(extend-type Slime
  actions
  (show [m]
    (print "A slime mold with a sliminess of" (:sliminess m)))
  (attack [m p]
    (let [s (randval (:sliminess m))
          d (if (zero? (rand-int 2)) 1 0)]
      (println "A slime mold wraps around your legs and decreases your agility by" s)
      (if (> d 0) (println "It also squirts in your face, taking away a health point!"))
      {:m m :p (hit (update-in p [:agility] - s) d)}))
  (hit [m d]
    (let [m-hit (update-in m [:health] - d)]
      (if (dead? m-hit)
        (println "You killed the slime mold!")
        (println "You hit the slime, knocking off" d "health point(s)!"))
      m-hit)))
(defn make-slime "Creates a new slime mold" [] (Slime. (randval 10) (randval 5)))

(defrecord Brigand [health])
(extend-type Brigand
  actions
  (show [m]
    (print "A fierce brigand"))
  (attack [m p]
    (let [h (:health p)
          a (:agility p)
          s (:strength p)
          x (max h a s)]
      (cond
       (= x h) (let [d 2]
                 (println "A brigand hits you with his slingshot, taking off" d "healt point(s)")
                 {:m m :p (hit p d)})
       (= x a) (let [d 2]
                 (println "A brigand catches your leg with his whip, taking off" d "agility point(s)")
                 {:m m :p (update-in p [:agility] - d)})
       (= x s) (let [d 2]
                 (println "A brigand cuts your arm with his whip, taking off" d "strength point(s)")
                 {:m m :p (update-in p [:strength] - d)}))))
  (hit [m d]
    (let [m-hit (update-in m [:health] - d)]
      (if (dead? m-hit)
        (println "You killed the brigand!")
        (println "You hit the brigand, knocking off" d "health point(s)!"))
      m-hit)))
(defn make-brigand "Creates a new brigand" [] (Brigand. (randval 10)))

(def monster-builders
  "Monster builder functions"
  (into [] (map resolve '(make-orc make-hydra make-slime make-brigand))))

(defn init-player
  "Convenience method to call the player constructor"
  []
  (make-player))

(defn init-monsters
  "Returns a vector of random monsters"
  []
  (let [b (count monster-builders)]
    (into [] (for [x (range monster-num)]
               ((eval (nth monster-builders (rand-int b))))))))

(defn player-round
  "Player round, recurses on number of attacks"
  [actors n]
  (let [p (:p actors)
        ms (:ms actors)
        outcome (attack p ms)
        p-new (:p outcome)
        ms-new (:ms outcome)]
    (if (or (dead? p-new)
            (monsters-dead? ms-new)
            (<= n 1))
      outcome
      (recur outcome (dec n)))))

(defn monsters-round
  "Monster round, recurses for each monster in the list"
  ([actors]
     (monsters-round actors 0))
  ([actors n]
     (let [p (:p actors)
           ms (:ms actors)
           m (ms n)
           combat (if (dead? m)
                    {:p p :m m}
                    (attack m p))
           p-new (:p combat)
           ms-new (assoc ms n (:m combat))
           outcome {:p p-new :ms ms-new}]
       (if (or (dead? p-new)
               (monsters-dead? ms-new)
               (>= (inc n) (count ms-new)))
         outcome
         (recur outcome (inc n))))))

(defn game-loop
  "Game turn"
  [p ms]
  (when-not (or (dead? p)
                (monsters-dead? ms))
    (show p)
    (let [a {:p p :ms ms}
          n (inc (quot (max 0 (:agility p)) 15))
          p-combat (player-round a n)
          m-combat (monsters-round p-combat)
          p-new (:p m-combat)
          ms-new (:ms m-combat)]
      (cond
       (dead? p-new) (println "Game over.")
       (monsters-dead? ms-new) (println "Congratulations! You have vanquished all of your foes.")
       :else (recur p-new ms-new)))))

(defn orc-battle
  "Run the game"
  []
  (println "\nWelcome to the ORC-BATTLE!")
  (game-loop (init-player) (init-monsters)))

;;(orc-battle)