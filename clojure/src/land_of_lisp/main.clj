(ns land-of-lisp.main
  (:require [land-of-lisp.orc-battle.core :as orc]
            [land-of-lisp.evolution.core :as evo]
            [land-of-lisp.wizard.core :as wiz])
  (:gen-class))

(def games
  (into [] (map resolve '(orc/orc-battle evo/evolution wiz/wizard))))
(def game-names
  ["orc-battle" "evolution" "wizard"])

(defn show-games
  "Show available games"
  [games]
  (println "Available games:")
  (dotimes [x (count games)]
    (let [g (games x)]
      (println (str "   " (inc x) ". " g)))))

(defn pick-game
  "Pick a game"
  [games]
  (println "Pick a game #: ")
  (let [n (read-line)]
    (if (re-matches #"[0-9]+" n)
      (let [x (read-string n)]
        (cond
         (and (>= x 1) (<= x (count games))) (dec x)
         :else (recur games)))
      (recur games))))

(defn -main
  "Main function to run the other games"
  [& args]
  (println "Land of lisp games in Clojure!")
  (println "Select the game:")
  (show-games game-names)
  ((eval (games (pick-game games)))))

;;(-main)
