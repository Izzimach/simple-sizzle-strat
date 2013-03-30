(ns simplestrat.gamestate
  (:require [clojure.set]))

;;
;; basic game state
;;

(defn makeemptygamestate []
  {
   ;; game map as a set of tiles
   :map nil
   ;; all current characters
   :characters {}
   ;; which team is active; either :team1 or :team2
   :activeteam :team2
   ;; a set of actions left for the active team. includes both move
   ;; actions and standard actions
   :actionsleft #{}
   ;; index of the current turn, used for effects with a duration
   :turn 0})

;;
;; turns
;;

(defn- nextturnteamfrom [currentturnteam]
  (if (= :team1 currentturnteam) :team2 :team1))

(defn- charactersforteam [gamestate team]
  (filter #(= team (:team %)) (vals (:characters gamestate))))

(defn- actionsforcharacter [character]
  (let [uniqueid (:uniqueid character)]
    #{ [:moveaction uniqueid] [:standardaction uniqueid] }))

(defn- actionsforteam
  "Produces a set of all the allowed actions for the specified team, by
  basically gathering up all the actions available to each member of
  the team."
  [gamestate team]
  (let [characters (charactersforteam gamestate team)]
    ;;(js/console.log (clj->js team))
    ;;(js/console.log (clj->js characters))
    (reduce #(clojure.set/union %1 (actionsforcharacter %2)) #{} characters))
  )

(defn advanceturn [gamestate]
  (let [nextteam (nextturnteamfrom (:activeteam gamestate))
        nextactions (actionsforteam gamestate nextteam)
        nextturn (inc (:turn gamestate))]
    ;;(js/console.log (clj->js actions))
    (assoc gamestate :activeteam nextteam :actionsleft nextactions :turn nextturn)))

;;
;; character management
;;
(defn createstandardattackaction [name icon range damage]
  {:action name :range range :damage damage :iconindex icon})

(defn createcharacter [charactername id iconindex x y team]
  {:name charactername
   :uniqueid id
   :iconindex iconindex
   :x x
   :y y
   :team team
   :health 3
   :move 2
   :actions [(createstandardattackaction "Default Attack" 67 2 2)]})

(defn update-addcharacter [gamestate freshcharacter]
  (assoc-in gamestate [:characters (:uniqueid freshcharacter)] freshcharacter))

(defn update-movecharacter [gamestate character newx newy]
  (let [characters (:characters gamestate)
        movedcharacter (-> character (assoc :x newx) (assoc :y newy))
        uniqueid (:uniqueid movedcharacter)]
    (assoc-in gamestate [:characters uniqueid] movedcharacter)))

(defn update-removecharacter [gamestate character]
  (clojure.contrib.core/dissoc-in gamestate [:characters (:uniqueid character)]))

(defn makestartingcharacters []
  [(createcharacter "bob" 1 128 5 2 :team2)
   (createcharacter "tom" 2 130 6 5 :team1)
   (createcharacter "shemp" 3 191 8 5 :team1)])

(defn addstartingcharacters [gamestate]
  (let [characters (makestartingcharacters)]
    (reduce update-addcharacter gamestate characters)))

(defn- istileopen? [gamestate [x y]]
  ;; blocked if a character is there
  (let [characters (vals (:characters gamestate))
        ischaracterat? (fn [char] (and (= x (:x char)) (= y (:y char))))]
    ;;(js/console.log movex y (clj->js characters))
    (not-any? ischaracterat? characters)))

(defn- allies? [character1 character2]
  (= (:team character1) (:team character2)))

(defn- enemies? [character1 character2]
  (not (allies? character1 character2)))

(defn moveactionavailablefor [gamestate character]
  (let [moveaction [:moveaction (:uniqueid character)]
        availableactions (:actionsleft gamestate)]
    ;; return true if the moveaction exists in the set of available
    ;;actions
    ;;(js/console.log (clj->js availableactions))
    (not (nil? (get availableactions moveaction)))))

(defn standardactionavailablefor [gamestate character]
  (let [attackaction [:standardaction (:uniqueid character)]
        availableactions (:actionsleft gamestate)]
    (not (nil? (get availableactions attackaction)))))

(defn seqof-movesforcharacter [gamestate character active-action]
  ;; no moves if the character has no available move action
  (if (moveactionavailablefor gamestate character)
    ;; just list adjacent locations
    (let [x (:x character)
          y (:y character)]
      (filter #(istileopen? gamestate %)
              (for [xd [-1 0 1]
                    yd [-1 0 1]
                    :let [mx (+ x xd)
                          my (+ y yd)]]
                [mx my])))
    ;; no move action available
    nil
    ))

(defn seqof-targetsforcharacter [gamestate character active-action]
  (seq []))

(defn seqof-actionsforcharacter [gamestate character active-action]
  (seq (:actions character)))

(defn getdefaultaction [character]
  ;; just get the first element in the action vector
  (-> character :actions (get 0)))

(defn getclickablesfor [gamestate character active-action]
  (let [moves (seqof-movesforcharacter gamestate character active-action)
        targets (seqof-targetsforcharacter gamestate character active-action)
        actions (seqof-actionsforcharacter gamestate character active-action)]
    {:moves moves :targets targets :actions actions}))

;;
;; terrain/map
;;

(defn chooseterrain [x y]
  (if (> x y)
    39
    54))

(defn makestartingmap [{:keys [width height]}]
  (for [x (range 0 width)
        y (range 0 height)]
    {:x x :y y :terrain (chooseterrain x y)}))

;;
;; initialization
;;

(defn makestartingstate []
  (let [gamemap (makestartingmap {:width 10 :height 10})
        emptygamestate (makeemptygamestate)]
    (-> emptygamestate
        (assoc :map gamemap)
        (addstartingcharacters)
        (advanceturn) ; so team1 basically starts first
        )))