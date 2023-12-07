(ns mire.player
  (:require [mire.rooms :as rooms]
            [mire.player :as player]))

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)


(def prompt "> ")
(def streams (ref {}))


(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))


(defn death []
  (println "You're dead")
  (ref-set player/*current-room* (@rooms/rooms :start))
  (ref-set player/*inventory* (ref {}))
  (ref-set rooms/rooms (ref {}))
  (rooms/add-rooms "resources/rooms"))

;;stats
(defn calc-attack
  [lvl monster]
  (cond
    (= monster "orc") (* lvl 4)
    (= monster "lizard") (* lvl 2)
    (= monster "skeleton") (* lvl 3)
    (= monster "rat") (* lvl 1)
    (= monster "demon") (* lvl 5)
    :else (* lvl 6)))
(defn calc-defence
  [lvl monster]
  (cond
    (= monster "orc") (int (* lvl 1.3))
    (= monster "lizard") (int (* lvl 1.1))
    (= monster "skeleton") (int (* lvl 1.2))
    (= monster "rat") (int (* lvl 1))
    (= monster "demon") (int (* lvl 1.3))
    :else (int (* lvl 1.4))))
(defn calc-hitpoints
  [lvl monster]
  (cond
    (= monster "orc") (* lvl (rand-nth [15 16 17 18]))
    (= monster "lizard") (* lvl (rand-nth [7 8 9 10]))
    (= monster "skeleton") (* lvl (rand-nth [11 12 13 14]))
    (= monster "rat") (* lvl (rand-nth [1 2 3 4 5 6]))
    (= monster "demon") (* lvl 19)
    :else (* lvl 20)))


(defn calc-sides
  [lvl]
  (if (> lvl 5) 4 6))
(defn kill-negative
  [n]
  (if (neg? n) 0 n))
(defn calc-base-damage
  [att def]
  (kill-negative (- att def)))
(defn roll-dice
  [sides]
  (inc (rand-int sides)))


(defn create-character
  [name lvl]
  {:name name
   :lvl lvl
   :att (calc-attack lvl name)
   :def (calc-defence lvl name)
   :hp (calc-hitpoints lvl name)})



(defn real-damage
  [base sides]
  (let [rd (roll-dice sides)
        s (/ sides 2)]
    (cond
      (<= rd s) (int (/ base 2))
      (> rd s) base
      (= rd sides) (* base 2))))


(defn take-damage
  [from to]
  (let [bd (calc-base-damage (:att from) (:def to))
        s (calc-sides (:lvl from))
        rd (real-damage bd s)]
    [rd (update-in to [:hp] #(- % rd))]))


;;player
(def player (create-character "player" 6))

;;monsters
(def lizard (create-character "lizard" 3))
(def skeleton (create-character "skeleton" 4))
(def orc (create-character "orc" 5))
(def rat (create-character "rat" 1))

;;boss
(def demon-boss (create-character "demon" 6))


(def log-template
  " %s received %d damage.
  his new life is %d")

(defn print-battle-log
  [damage character]
  (let [name (:name character)
        newhp (:hp character)
        s (format log-template name damage newhp)]
    (println s)))


(defn print-winner
  [p-hp e-hp]
  (if (<= p-hp 0)
    ((println "Enemy won.") ((death))
    ((println "You won!!!")))))


(defn battle
  [config]
  (loop [player (:player config)
         enemy ((rand-nth [:orc :lizard :skeleton :rat]) config)
         round 1]

    (if (carrying? (keyword "sword"))
      (if (= (:name @player/*current-room*) :throneroom)
        (if (carrying? (keyword "cross"))
          ((println "Just one look at the monster is enough to send shivers down your spine. You feel the dark energy emanating from this creature, but the raised cross prevents it from breaking you.") 
           (loop [player (:player config)
                  enemy (:demon-boss config)
                  round 1]

             (if (or (<= (:hp player) 0)
                     (<= (:hp enemy) 0))
               ((print-winner (:hp player) (:hp enemy))
                (println "Congratulations! You have defeated the boss and completed the game!"))
               (let [pl->en (take-damage player enemy)
                     en->pl (take-damage enemy player)]
                 (do (println (str "Round " round ":"))
                     (print-battle-log (pl->en 0) (pl->en 1))
                     (print-battle-log (en->pl 0) (en->pl 1))
                     (recur (en->pl 1) (pl->en 1) (inc round)))))))
          ((println "Just one look at the monster is enough to send shivers down your spine. You feel the dark energy emanating from this creature, crushing your entire body. Unable to cope with it, you fall motionless.")
           (death)))
        (if (or (<= (:hp player) 0)
                (<= (:hp enemy) 0))
          (print-winner (:hp player) (:hp enemy))
          (let [pl->en (take-damage player enemy)
                en->pl (take-damage enemy player)]
            (do (println (str "Round " round ":"))
                (print-battle-log (pl->en 0) (pl->en 1))
                (print-battle-log (en->pl 0) (en->pl 1))
                (recur (en->pl 1) (pl->en 1) (inc round))))))
      ((println "You encountered a monster and had nothing left but to die.")
       (death)))))


;; (defn final-battle
;;   [config]
;;   (if (carrying? (keyword "cross"))
;;     ((println "Just one look at the monster is enough to send shivers down your spine. You feel the dark energy emanating from this creature, but the raised cross prevents it from breaking you.")
;;      (loop [player (:player config)
;;             enemy ((rand-nth [:orc :lizard :skeleton :rat]) config)
;;             round 1]
       
;;        (if (or (<= (:hp player) 0)
;;                (<= (:hp enemy) 0))
;;          (print-winner (:hp player) (:hp enemy))
;;          (let [pl->en (take-damage player enemy)
;;                en->pl (take-damage enemy player)]
;;            (do (println (str "Round " round ":"))
;;                (print-battle-log (pl->en 0) (pl->en 1))
;;                (print-battle-log (en->pl 0) (en->pl 1))
;;                (recur (en->pl 1) (pl->en 1) (inc round)))))))
;;     ((println "Just one look at the monster is enough to send shivers down your spine. You feel the dark energy emanating from this creature, crushing your entire body. Unable to cope with it, you fall motionless.")
;;      death)))



