(ns land-of-lisp.wizard.core
  (:require [clojure.tools.reader.edn :as edn :only [read-string]]
            [clojure.string :as cs :only [join]])
  (:gen-class))

;; data
(def nodes
  {:living-room "You are in the living-room. A wizard is snoring loudly on the couch."
   :garden "You are in a beautiful garden. There is a well in front of you."
   :attic "You are in the attic. There is a giant welding torch in the corner."})

(def edges
  {:living-room [[:garden :west :door]
                 [:attic :upstairs :ladder]]
   :garden [[:living-room :east :door]]
   :attic [[:living-room :downstairs :ladder]]})

(def objects
  (ref {:whiskey :living-room
        :bucket  :living-room
        :frog    :garden
        :chain   :garden}))

(def location (atom :living-room))

(def allowed-commands (atom '(look walk pickup inventory)))

;; descriptions
(defn describe-location [loc nodes]
  (loc nodes))

(defn describe-path [edge]
  (->> ["There is a" (name (nth edge 2))
        "going" (name (nth edge 1))
        "from here."]
       (cs/join \space)))

(defn describe-paths [loc edges]
  (cs/join \space (map describe-path (loc edges))))

(defn is-at [obj loc objs]
  (= loc (obj objs)))

(defn objects-at [loc objs]
  (filter #(is-at % loc objs) (keys objs)))

(defn describe-object [obj]
  (str "You see a " (name obj) " on the floor."))

(defn describe-objects [loc objs]
  (cs/join \space (map describe-object (objects-at loc objs))))

;; base actions
(defn look []
  (->> [(describe-location @location nodes)
        (describe-paths @location edges)
        (describe-objects @location @objects)]
       (remove empty?)
       (cs/join \newline)))

(defn walk [direction]
  (let [correct-way (fn [edge] (= (nth edge 1) direction))
        next (nth (first (filter correct-way (@location edges))) 0)]
    (if next
      (do
        (swap! location (fn [_] next))
        (look))
      "You cannot go that way.")))

(defn pickup [object]
  (if (is-at object @location @objects)
    (do
      (dosync (alter objects assoc object :body))
      (str "You are now carrying the " (name object) "."))
    "You cannot get that."))

(defn inventory []
  (let [inv (objects-at :body @objects)]
    (if (empty? inv)
      "You are not carrying anything."
      (str "You are carrying: " (cs/join ", " (map name inv)) "."))))

;; repl functions
(defn valid-command? [cmd]
  (some #(= cmd %) @allowed-commands))

(defn game-read [request-prompt request-exit]
  (or ((clojure.main/skip-whitespace *in*) {:line-start request-prompt
                                            :stream-end request-exit})
      (let [cmd (edn/read-string (str "(" (read-line) ")"))
            command (first cmd)]
        (cond
         (or (= command 'exit) (= command 'quit)) request-exit
         (valid-command? command) (cons command
                                        (map #(list 'keyword (list 'quote %))
                                             (rest cmd)))
         :else (do (println "I do not know that command.") request-prompt)))))

(defn game-eval [sexp]
  (if (valid-command? (first sexp))
    (eval sexp)))

(defn game-print [lst]
  (newline)
  (println lst)
  (newline))

(defn game-prompt []
  (print "Your action: "))

(defn game-repl []
  (clojure.main/repl
   :init (fn [] (use 'land-of-lisp.wizard.core))
   :prompt game-prompt
   :print game-print
   :read game-read
   :eval game-eval
   :caught (fn [e] (println "Something went wrong...\n" e))))

;; game actions
(defn have [object]
  (is-at object :body @objects))

(defmacro game-action [command subj obj place & body]
  `(do (defn ~command [subject# object#]
         (if (and (= @location ~place)
                  (= subject# ~subj)
                  (= object# ~obj)
                  (have ~subj))
           ~@body
           (str "I cant " '~command " like that.")))
       (swap! allowed-commands conj '~command)))

(def chain-welded (atom false))

(game-action weld :chain :bucket :attic
             (if (and (have :bucket) (not @chain-welded))
               (do (swap! chain-welded (fn [_] true))
                   "The chain is now securely welded to the bucket.")
               "You do not have a bucket."))

(def bucket-filled (atom false))

(game-action dunk :bucket :well :garden
             (if @chain-welded
               (do (swap! bucket-filled (fn [_] true))
                   "The bucket is now full of water")
               "The water level is too low to reach."))

(def lose-game
  "The wizard awakens and sees that you stole his frog.
He is so upset he banishes you to the netherworlds - you lose! The end.")

(def win-game
  "The wizard awakens from his slumber and greets you warmly.
He hands you the magic low-carb donut - you win! The end.")

(game-action splash :bucket :wizard :living-room
             (cond (not @bucket-filled) "The bucket has nothing in it."
                   (have :frog) lose-game
                   :else win-game))

(defn wizard
  "Run the game"
  []
  (println "Welcome to Wizard's game!")
  (game-repl))

;; main
;;(defn -main [& args]
;;  (game-repl))
