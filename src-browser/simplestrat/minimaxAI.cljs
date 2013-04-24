(ns simplestrat.minimaxAI
  (:require [simplestrat.gameworld :as world]
            [simplestrat.action :as action]))

;;
;; the game evaluation function takes a given gamestate
;; and produces a number indicating how good that state is,
;; with more positive values favoring team1 and more negative values
;; favoring team 2
(def *gameevaluationfunction*)

(defn totalhealthforteam [team]
  (reduce + (map :health team)))

(defn basichealthevaluation
  "Evaluates the game state by adding up and comparing the total health
  on each side."
  [gamestate]
  (let [team1characters (world/charactersforteam gamestate :team1)
        team2characters (world/charactersforteam gamestate :team2)]
    (- (totalhealthforteam team1characters) (totalhealthforteam team2characters))))

(defn basicheadcountevaluation
  "Evaluates the game state by counting how many characters are
  still alive on each side."
  [gamestate]
  (let [team1characters (world/charactersforteam gamestate :team1)
        team2characters (world/charactersforteam gamestate :team2)]
    (- (count team1characters) (count team2characters))))

(defn basicevaluation [gamestate]
  (let [healtheval (basichealthevaluation gamestate)
        headcounteval (basicheadcountevaluation gamestate)
        ;; dictate how much each factor affects the final number
        healthfactor 1
        headcountfactor 0.1]
    (+
      (* healtheval healthfactor)
      (* headcounteval headcountfactor))))

;; use the basic evaluation by default
(set! *gameevaluationfunction* basicevaluation)


(defn- expandmove [gamestate move]
  (let [characterid (get move 1)
        character (world/get-character gamestate characterid)
        makemoveinstance (fn [[actiondata movelocation]] 
                           {:character character :actiondata actiondata :args movelocation})]
    (map makemoveinstance (action/seqof-movelocationsforcharacter gamestate character nil))))

(defn- expandattack [gamestate attack]
  (let [characterid (get attack 1)
        character (world/get-character gamestate characterid)
        makeattackinstance (fn [[actiondata target]]
                             {:character character :actiondata actiondata :args [target]})]
    (map makeattackinstance (action/seqof-attacktargetsforcharacter gamestate character nil))))

(defn- expandaction [gamestate action]
  (if (= :moveaction (get action 0))
    (expandmove gamestate action)
    (expandattack gamestate action)))

(defn- possibleactions
  "For the given gamestate produces a sequence of all possible actions available."
  [gamestate]
  (let [activeteam (:activeteam gamestate)
        availableactiontypes (:actionsleft gamestate)]
    (apply concat (map #(expandaction gamestate %) availableactiontypes))))


(defn- findbestaction [gamestate availableactions]
  (let [;; maximize or minimize the evaluation function depending on which team is active
        choosebest (fn [comparefunc a b] (let [[aaction avalue] a
                                  [baction bvalue] b]
                              (if (comparefunc avalue bvalue) a b)))
        reducer (if (= :team1 (:activeteam gamestate))
                  (partial choosebest >)  ; team1 maximizes
                  (partial choosebest <)) ; team2 minimizes
        evaluateaction (fn [action] [action (*gameevaluationfunction* (action/invokeactioninstance action gamestate))])]
    (first (reduce reducer (map evaluateaction availableactions)))))

(defn- findbestaction-wholeturn [gamestate lookahead]
  (let [availableactions (possibleactions gamestate)]
    (if (empty? availableactions)
      ;; reached at the end of turn (no more actions available)
      ;; check to see if we need to look ahead to the next turn, or stop here
      (if (= 0 lookahead)
        [nil (*gameevaluationfunction* gamestate)]
        (findbestaction-wholeturn (world/advanceturn gamestate) (dec lookahead)))
      ;; not at the end of turn. Try all the possible actions, and recursively
      ;; check each of those
      (let [evaluateaction (fn [action]
                             (let [[restactions rating] (findbestaction-wholeturn (action/invokeactioninstance action gamestate) lookahead)]
                               [(cons action restactions) rating]))
            ;; maximize or minimize the evaluation function depending on which team is active
            choosebest (fn [comparefunc a b] (let [[aactions avalue] a
                                                   [bactions bvalue] b]
                                               (if (comparefunc avalue bvalue) a b)))
            reducebest (if (= :team1 (:activeteam gamestate))
                      (partial choosebest >)  ; team1 maximizes
                      (partial choosebest <)) ; team2 minimizes
            ]
        (reduce reducebest (map evaluateaction availableactions))))))

(defn- completeturn
  "If there are still actions left this turn, figure out which ones to use and in what order, based
  on the values produce by the gamestate evaluation function. Generates a list of actions."
  [gamestate]
  (let [availableactions (possibleactions gamestate)
        bestaction (findbestaction gamestate availableactions)]
    (if (nil? bestaction)
      []
      (let [newstate (action/invokeactioninstance bestaction gamestate)]
        (cons bestaction (completeturn newstate))))))

(defn- eataction [gamestate]
  (let [availableactions (possibleactions gamestate)
        bestaction (findbestaction gamestate availableactions)]
    (if (nil? bestaction)
      nil
      [bestaction (action/invokeactioninstance bestaction gamestate)])))

(defn- computeractions
  "Computes the desired computer actions, and returns them as a 
  sequence of functions to call with the gamestate as the argument."
  [gamestate computerteam]
  ;; The AI generate various hypothetical game states to try out various
  ;; actions. We don't want to generate log messages or other side effects while
  ;; generating this game states, so we turn off the message log and
  ;; other effects
  (let [virtualgamestate (world/disablemessagelog gamestate)
        [bestactions actionsrating] (findbestaction-wholeturn virtualgamestate 0)]
    bestactions
    ))


(defn executecomputerturn
  [gamestate]
  (let [computerteam (:activeteam gamestate)
        actions (computeractions gamestate computerteam)
        invokeaction (fn [gamestate actioninstance] (action/invokeactioninstance actioninstance gamestate))]
    (reduce invokeaction gamestate actions)))
