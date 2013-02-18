(ns simplestrat.gamestate
  (:require [clojure.set]))

;;
;; turns
;;

(defn- nextturnowner [currentowner]
  (if (= :team1 currentowner)
    :team2
    :team1))

(defn- charactersforteam [gamestate team]
  (filter #(= team (:team %))
          (vals (:characters gamestate))))

(defn- actionsforcharacter [character]
  (let [uniqueid (:uniqueid character)]
    #{ [:moveaction uniqueid] [:attackaction uniqueid] }))

(defn- actionsforteam [gamestate team]
  ;; map keys are character ids, values are a set of actions that
  ;; character can perform
  (let [characters (charactersforteam gamestate team)]
    ;;(js/console.log (clj->js team))
    ;;(js/console.log (clj->js characters))
    (reduce #(clojure.set/union %1 (actionsforcharacter %2)) #{} characters))
  )

(defn advanceturn [gamestate]
  (let [nextteam (nextturnowner (:activeteam gamestate))
        actions (actionsforteam gamestate nextteam)]
    ;;(js/console.log (clj->js actions))
    (assoc gamestate :activeteam nextteam :actionsleft actions)))

;;
;; character management
;;

(defn createcharacter [charactername id iconindex x y team]
  {:name charactername :uniqueid id :iconindex iconindex :x x :y y :team team})

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

(defn- enemies? [character1 character2]
  (not= (:team character1) (:team character2)))

(defn moveactionavailablefor [gamestate character]
  (let [moveaction [:moveaction (:uniqueid character)]
        availableactions (:actionsleft gamestate)]
    ;; return true if the moveaction exists in the set of available
    ;;actions
    ;;(js/console.log (clj->js availableactions))
    (not (nil? (get availableactions moveaction)))))

(defn attackactionavailablefor [gamestate character]
  (let [attackaction [:attackaction (:uniqueid character)]
        availableactions (:actionsleft gamestate)]
    (not (nil? (get availableactions attackaction)))))

(defn seqof-movesforcharacter [gamestate character mode]
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

(defn seqof-attacksforcharacter [gamestate character mode]
  (seq []))

(defn seqof-modesforcharacter [gamestate character mode]
  (seq []))

(defn getclickablesfor [gamestate character mode]
  (let [moves (seqof-movesforcharacter gamestate character mode)
        attacks (seqof-attacksforcharacter gamestate character mode)
        modes (seqof-modesforcharacter gamestate character mode)]
    {:moves moves :attacks attacks :modes modes}))

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
        characters (makestartingcharacters)
        emptygamestate {:map nil :characters {} :activeteam :team2 :actionsleft {}}]
    (-> emptygamestate
        (assoc :map gamemap)
        (addstartingcharacters)
        (advanceturn) ; so team1 basically starts first
        ))) 