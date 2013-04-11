(ns simplestrat.actions
  (:require [simplestrat.gameworld :as world]))

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

(defn invokemoveaction [gamestate character moveaction [destx desty]]
  ;; for now the moveaction is ignored, we just move the relevant
  ;; character
  (world/move-character gamestate (:uniqueid character) destx desty)
  )

(defn invokedefaultmoveaction [gamestate character [destx desty]]
  (let [moveaction (take 1 (seqof-charactermoveactions character))]
    (invokemoveaction gamestate character moveaction [destx desty])))

(defn invokemajoraction [gamestate character majoraction targets]
  ;; just apply damage to the target
  (let [damage (:damage majoraction)]
    (world/damage-character gamestate (:uniqueid character) damage nil)
    )
  )
