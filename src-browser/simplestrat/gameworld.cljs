(ns simplestrat.gameworld
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
    #{ [:moveaction uniqueid] [:majoraction uniqueid] }))

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

(defn advanceturn "Advances the game world to the next turn, allocating move and major actions for the newly-active team"
  [gamestate]
  (let [nextteam (nextturnteamfrom (:activeteam gamestate))
        nextactions (actionsforteam gamestate nextteam)
        nextturn (inc (:turn gamestate))]
    ;;(js/console.log (clj->js actions))
    (assoc gamestate :activeteam nextteam :actionsleft nextactions :turn nextturn)))

(defn moveactionavailablefor? [gamestate characterid]
  (let [moveaction [:moveaction characterid]
        availableactions (:actionsleft gamestate)]
    ;; return true if the moveaction exists in the set of available
    ;;actions
    ;;(js/console.log (clj->js availableactions))
    (not (nil? (get availableactions moveaction)))))

(defn majoractionavailablefor? [gamestate characterid]
  (let [majoraction [:majoraction characterid]
        availableactions (:actionsleft gamestate)]
    (not (nil? (get availableactions majoraction)))))

(defn get-character [gamestate characterid]
  (get-in gamestate [:characters characterid]))

(defn put-character [gamestate freshcharacter]
  (assoc-in gamestate [:characters (:uniqueid freshcharacter)] freshcharacter))

(defn move-character [gamestate characterid newx newy]
  (let [character (get-character gamestate characterid)
        movedcharacter (-> character (assoc :x newx) (assoc :y newy))]
    (put-character gamestate movedcharacter)))

(defn remove-character [gamestate characterid]
  (clojure.contrib.core/dissoc-in gamestate [:characters characterid]))

(defn- istileopen? [gamestate [x y]]
  ;; blocked if a character is there
  (let [characters (vals (:characters gamestate))
        ischaracterat? (fn [char] (and (= x (:x char)) (= y (:y char))))]
    ;;(js/console.log movex y (clj->js characters))
    (not-any? ischaracterat? characters)))


;;
;; character management
;;

(defn create-character [{:keys [charactername id iconindex coords team starthealth actions]}]
  (let [[x y] coords]
    {:name charactername
     :uniqueid id
     :iconindex iconindex
     :x x
     :y y
     :team team
     :health starthealth
     :shield 0
     :actions actions
     }))

(defn damage-character [gamestate characterid damageamount damagetype]
  (let [character (get-character gamestate characterid)
        newhealth (max 0 (- (:health character) damageamount))
        damagedcharacter (assoc character :health newhealth)]
    ;; if this defeats the character, remove them from the world
    (if (= 0 newhealth)
      (remove-character gamestate characterid)
      (put-character gamestate damagedcharacter))
    )
  )

(defn- allies? [character1 character2]
  (= (:team character1) (:team character2)))

(defn- enemies? [character1 character2]
  (not (allies? character1 character2)))



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
