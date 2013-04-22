(ns simplestrat.minimaxAI
  (:require [simplestrat.gameworld :as world]
            [simplestrat.action :as action]))

(defn- expandmove [gamestate move]
  (let [characterid (get move 1)
        character (world/get-character gamestate characterid)]
    (action/seqof-movelocationsforcharacter gamestate character nil)))

(defn- expandattack [gamestate attack]
  (let [characterid (get attack 1)
        character (world/get-character gamestate characterid)]
    (action/seqof-attacktargetsforcharacter gamestate character nil)))

(defn- expandaction [gamestate action]
  (if (= :moveaction (get action 0))
    (expandmove gamestate action)
    (expandattack gamestate action)))

(defn- possibleactions
  "For the given gamestate produces a sequence of all possible actions available."
  [gamestate]
  (let [activeteam (:activeteam gamestate)
        availableactiontypes (world/actiontypesavailableforteam gamestate activeteam)]
    (apply concat (map #(expandaction gamestate %) availableactiontypes))))



(defn- computeractions
  "Computes the desired computer actions, and returns them as a 
  sequence of functions to call with the gamestate as the argument."
  [gamestate computerteam]
  ;; The AI generate various hypothetical game states to try out various
  ;; actions. We don't want to generate log messages or other side effects while
  ;; generating this game states, so we turn off the message log and
  ;; other effects
  (let [virtualgamestate (world/disablemessagelog gamestate)]
    ))


(defn executecomputerturn
  [gamestate]
  (let [computerteam (:activeteam gamestate)
        actions (computeractions gamestate computerteam)]
    (reduce #(%1 %2) gamestate actions)))
