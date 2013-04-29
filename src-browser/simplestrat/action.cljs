(ns simplestrat.action
  (:require [simplestrat.gameworld :as world]
            [clojure.string :as string]))


(defrecord MajorActionData [name uniqueid iconindex range damage description])

(defrecord MoveActionData [name uniqueid iconindex speed description])

(defrecord ActionInstance [character actiondata args])

;;
;; character actions
;;

(defn createmajoraction [name uniqueid iconindex range damage description]
  #_{:actiontype :majoraction :name name :range range :damage damage :iconindex icon}
  (->MajorActionData name uniqueid iconindex range damage description)
  )

(defn createmoveaction [name uniqueid iconindex movespeed description]
  #_{:actiontype :moveaction :name name :speed movespeed :iconindex icon}
  (->MoveActionData name uniqueid iconindex movespeed description)
  )

(defn- ismoveaction? [action]
  #_(= :moveaction (:actiontype action))
  (instance? MoveActionData action))

(defn- ismajoraction? [action]
  #_(= :majoraction (:actiontype action))
  (instance? MajorActionData action))

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
    (filter (fn [coords] (world/istileopen? gamestate coords))
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
  (let [enemies (filter #(world/enemies? % character) (vals (:characters gamestate)))
        ;; add 0.5 to make sure adjacent diagonals are range 1
        attackrange (+ 0.5 (:range majoraction))
        checkrange (fn [target] (< (characterrange character target) attackrange))]
    (filter #(checkrange %) enemies)
    ))

(defn seqof-movelocationsforcharacter
  "Returns a sequence of [a c] paris, where a indicates the moveaction used and c
  contains the [x y] coordinates to move to. The third argument is an
  move action to use to produce the possible move locations. If no third argument is
  passed in, this function returns locations for all move actions this character has."
  [gamestate character & [selectedmoveactions]]
  ;; TODO: if selectedmoveaction is non-nil, use results from only
  ;; that action
  (let [moveactions (if (nil? selectedmoveactions) 
                      (seqof-charactermoveactions character)
                      [selectedmoveactions])]
    (apply concat  ;; thanks stackoverflow!
           (for [moveaction moveactions
                 :let [destinations (seqof-movedestinations gamestate character moveaction)]]
             ;; generate a vector of pairs: [a,b] where a=destination tile,
             ;; b=action used to move to that tile
             (map (fn [coords] [moveaction coords]) destinations)))))

(defn seqof-attacktargetsforcharacter
  "Returns a sequence of [a t] pairs where a indicates the major action used and
  t indicates the target character or [x y] tile location targeted by the action.
  If a third argument containing a specific major actionis specified, targets are computed
  only for that action. If no third argument is specified targets are computed for all actions
  available to this character."
  [gamestate character & [selectedmajoraction]]
  ;; if selectedmajoraction is non-nil, use results from only
  ;; that action
  (let [majoractions (if (nil? selectedmajoraction)
                       (seqof-charactermajoractions character)
                       [selectedmajoraction])]
    (apply concat
           (for [majoraction majoractions
                 :let [targets (seqof-targets gamestate character majoraction)]]
             ;; in the end we want a list of [action target] pairs
             (map #(vec [majoraction %]) targets)))))

(defn getdefaultmajoraction [character]
  (first (seqof-charactermajoractions character)))

(defn getdefaultmoveaction [character]
  (first (seqof-charactermoveactions character)))


(defn invokemoveaction [gamestate character moveaction [destx desty]]
  (let [moverange (:speed moveaction)
        characterid (:uniqueid character)]
    ;; for now the moveaction is ignored, we just move the relevant
    ;; character
    (-> gamestate
      (world/logmessage (string/join [(:name character) " moved."]))
      (world/move-character characterid destx desty)
      (world/consumeaction characterid :moveaction)
      ))
  )

(defn invokemajoraction [gamestate character majoraction targets]
  ;; just apply damage to the target
  (let [damage (:damage majoraction)
        characterid (:uniqueid character)
        damagetarget (fn [curstate target]
                       (-> curstate 
                           (world/logmessage (string/join [(:name character) " attacks " (:name target)]))
                           (world/damage-character (:uniqueid target) damage (:name majoraction))))
        damagetargets (fn [curstate targets] (reduce damagetarget curstate targets))]
    (-> gamestate
        (damagetargets targets)
        (world/consumeaction characterid :majoraction))
    )
  )

;;
;; action instances basically wrap the action specifics into a data structure (map) that
;; can be used as a generic object that modifies gamestate, that is "apply this action to the gamestate"
;; while still being able to access specifics like the action type or gamestate
;;
(defn invokeactioninstance
  [actioninstance gamestate]
  (let [character (.-character actioninstance)
        actiondata (.-actiondata actioninstance)
        args (.-args actioninstance)
        ;;{:keys [character actiondata args]} actioninstance
        ]
    (if (ismoveaction? actiondata)
      (invokemoveaction gamestate character actiondata args)
      (invokemajoraction gamestate character actiondata args))))


(defn invokedefaultmoveaction [gamestate character destination]
  (invokemoveaction gamestate character (getdefaultmoveaction character) destination))

(defn invokedefaultmajoraction [gamestate character targets]
  (invokemajoraction gamestate character (getdefaultmajoraction character) targets))
