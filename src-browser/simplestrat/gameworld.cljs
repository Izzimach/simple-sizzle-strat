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

(defn- allies? [character1 character2]
  (= (:team character1) (:team character2)))

(defn- enemies? [character1 character2]
  (not (allies? character1 character2)))


;;
;; character actions
;;

(defn createmajoraction [name icon range damage]
  {:action :major :name name :range range :damage damage :iconindex icon})

(defn createmoveaction [name icon movespeed]
  {:action :move :name name :speed movespeed :iconindex icon})

(defn- ismoveaction? [action]
  (= :move (:action action)))

(defn- ismajoraction? [action]
  (= :major (:action action)))

(defn seqof-charactermoveactions [character]
  (filter ismoveaction? (:actions character)))

(defn seqof-charactermajoractions [character]
  (filter ismajoraction? (:actions character)))

(defn- moverange [startcoord speed]
  (let [lowcoord (- startcoord speed)
        highcoord (+ startcoord speed 1)]
    (range lowcoord highcoord)))

(defn seqof-movedestinations [gamestate character moveaction]
  (let [speed (:speed moveaction)
        x (:x character)
        y (:y character)]
    (filter (fn [coords] (istileopen? gamestate coords))
            (for [destx (moverange x speed)
                  desty (moverange y speed)]
              [destx desty]))))

(defn- characterrange [character1 character2]
  (let [x1 (:x character1)
        y1 (:y character1)
        x2 (:x character2)
        y2 (:y character2)
        dx (- x1 x2)
        dy (- y1 y2)
        sqr #(* % %)]
    (Math/sqrt (+ (sqr dx) (sqr dy)))
    ))

(defn seqof-targets [gamestate character majoraction]
  ;; all enemies are valid targets
  (let [enemies (filter #(enemies? % character) (vals (:characters gamestate)))
        ;; add 0.5 to make sure adjacent diagonals are range 1
        attackrange (+ 0.5 (:range majoraction))
        checkrange (fn [target] (< (characterrange character target) attackrange))]
    (filter #(checkrange %) enemies)
    ))

(defn seqof-movelocationsforcharacter [gamestate character selectedmoveaction]
  ;; TODO: if selectedmoveaction is non-nil, use results from only
  ;; that action
  (apply concat  ;; thanks stackoverflow!
         (for [moveaction (seqof-charactermoveactions character)
               :let [ destinations (seqof-movedestinations gamestate character moveaction)]]
           ;; generate a vector of pairs: [a,b] where a=destination tile,
           ;; b=action used to move to that tile
           (map (fn [coords] [moveaction coords]) destinations))))

(defn seqof-attacktargetsforcharacter [gamestate character selectedmajoraction]
  ;; TODO: if selectedmajoraction is non-nil, use results from only
  ;; that action
  (apply concat
         (for [majoraction (seqof-charactermajoractions character)
               :let [targets (seqof-targets gamestate character majoraction)]]
           ;; in the end we want a list of [action target] pairs
           (map (fn [target] [majoraction target]) targets))))

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

(defn makestartingcharacters []
  [
   (create-character
    {:charactername "bob" :id 1 :iconindex 128 :coords [5 2] :team :team2 :starthealth 2 :actions [(createmoveaction "walk" 1 1)]})
   (create-character
    {:charactername "tom" :id 2 :iconindex 130 :coords [6 5] :team :team1 :starthealth 2 :actions [(createmoveaction "walk" 1 1)]})
   (create-character
    {:charactername "shemp" :id 3 :iconindex 191 :coords [8 5] :team :team1 :starthealth 2 :actions [(createmoveaction "run" 26 2)]})
   ])

(defn addstartingcharacters [gamestate]
  (let [characters (makestartingcharacters)]
    (reduce put-character gamestate characters)))

(defn makestartingstate []
  (let [gamemap (makestartingmap {:width 10 :height 10})
        emptygamestate (makeemptygamestate)]
    (-> emptygamestate
        (assoc :map gamemap)
        (addstartingcharacters)
        (advanceturn) ; so team1 basically starts first
        )))