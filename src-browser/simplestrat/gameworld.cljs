(ns simplestrat.gameworld
  (:require [clojure.set]
            [simplestrat.utils :as utils]
            [clojure.string :as string]))


(declare logmessage)
;;
;; basic game state
;;

(defrecord GameState [map characters activeteam actionsleft turn loglist])

(defn makeemptygamestate []
  (->GameState nil {} :team2 #{} 0 (list))
  #_{
   ;; game map as a set of tiles
   :map nil
   ;; all current characters
   :characters {}
   ;; which team is active; either :team1 or :team2
   :activeteam :team2
   ;; a set of actions available to the active team. includes both move
   ;; actions and standard actions
   :actionsleft #{}
   ;; index of the current turn, used for effects with a duration
   :turn 0
   :loglist (list)}
  )


;;
;; turns
;;

(defn- nextturnteamfrom [currentturnteam]
  (if (= :team1 currentturnteam) :team2 :team1))

(defn- stringfromteam [team]
  (if (= :team1 team)
    "team 1"
    "team 2"))

(defn charactersforteam
  "Generates a sequence of all the characters that belong on a specified team." [gamestate team]
  (filter #(= team (:team %)) (vals (:characters gamestate))))

(defn- actiontypesavailableforcharacter [character]
  (let [uniqueid (:uniqueid character)]
    #{ [:moveaction uniqueid] [:majoraction uniqueid] }))

(defn- actiontypesavailableforteam
  "Produces a set of all the allowed actions for the specified team, by
  basically gathering up all the actions available to each member of
  the team."
  [gamestate team]
  (let [characters (charactersforteam gamestate team)]
    ;;(js/console.log (clj->js team))
    ;;(js/console.log (clj->js characters))
    (reduce #(clojure.set/union %1 (actiontypesavailableforcharacter %2)) #{} characters))
  )

(defn advanceturn
  "Advances the game world to the next turn, generating available move and major actions for the newly-active team."
  [gamestate]
  (let [nextteam (nextturnteamfrom (:activeteam gamestate))
        nextactions (actiontypesavailableforteam gamestate nextteam)
        nextturn (inc (:turn gamestate))]
    ;;(js/console.log (clj->js actions))
    (-> gamestate
      (assoc :activeteam nextteam :actionsleft nextactions :turn nextturn)
      (logmessage (clojure.string/join ["Turn for " (stringfromteam nextteam) " begins."])))))

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

(defn consumeaction
  "Remove the specified action from the list of available actions, to indicate that the
  character has already performed this action."
  [gamestate characterid actiontype]
  (let [availableactions (:actionsleft gamestate)
        postconsumeactions (disj availableactions [actiontype characterid])]
    (assoc gamestate :actionsleft postconsumeactions)))

(defn get-character [gamestate characterid]
  (get-in gamestate [:characters characterid]))

(defn put-character [gamestate freshcharacter]
  (assoc-in gamestate [:characters (:uniqueid freshcharacter)] freshcharacter))

(defn move-character [gamestate characterid newx newy]
  (let [character (get-character gamestate characterid)
        movedcharacter (-> character (assoc :x newx) (assoc :y newy))]
    (put-character gamestate movedcharacter)))

(defn remove-character [gamestate characterid]
  (utils/dissoc-in gamestate [:characters characterid]))

(defn- istileopen? [gamestate [x y]]
  ;; blocked if a character is there
  (let [characters (vals (:characters gamestate))
        ischaracterat? (fn [char] (and (= x (:x char)) (= y (:y char))))]
    ;;(js/console.log movex y (clj->js characters))
    (and
      (not-any? ischaracterat? characters) ;; no characters blocking the tile
      (> x -1)
      (< x (-> gamestate :map :width))
      (> y -1)
      (< y (-> gamestate :map :height))
      )))

;;
;; message/event log
;;

(defn disablemessagelog
  "Messages won't get generated/stored for this gamestate" [gamestate]
  (dissoc gamestate :loglist))

(defn enablemessagelog
  "Messages will get generated/stored for this gamestate" [gamestate]
  ;; start with an empty list
  (assoc gamestate :loglist (list)))

(defn logmessage
  "Store a text message into the message log. The message log is displayed to the player."
  [gamestate message]
  (let [{:keys [loglist loglistmaxsize]} gamestate]
    ;; maintain some finite amount of log messages
    (if (not (nil? loglist))
      (assoc gamestate :loglist (conj loglist message))
      gamestate)))

;;
;; character management
;;

(defrecord Character [name uniqueid iconindex x y team health shield actions])

(defn create-character [{:keys [charactername id iconindex coords team starthealth actions]}]
  (let [[x y] coords]
    #_{:name charactername
     :uniqueid id
     :iconindex iconindex
     :x x
     :y y
     :team team
     :health starthealth
     :shield 0
     :actions actions
     }
    (map->Character {:name charactername :uniqueid id :x x :y y :iconindex iconindex :team team :health starthealth :shield 0 :actions actions})
    ))

(defn damage-character [gamestate characterid damageamount damagetype]
  (let [character (get-character gamestate characterid)
        newhealth (max 0 (- (:health character) damageamount))
        damagedcharacter (assoc character :health newhealth)
        damagelogtext (string/join [(:name character) " takes " damageamount " damage."])]
    ;; if this defeats the character, remove them from the world
    (if (= 0 newhealth)
      (-> gamestate
          (logmessage damagelogtext)
          (logmessage (string/join [(:name character) " is defeated!"]))
          (remove-character characterid))
      (-> gamestate
          (logmessage damagelogtext)
          (put-character damagedcharacter)))
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
  (let [tilecontents   (for [x (range 0 width)
                             y (range 0 height)]
                         {:x x :y y :terrain (chooseterrain x y)})
        ]
    {:tiledata tilecontents :width width :height height})
  )
