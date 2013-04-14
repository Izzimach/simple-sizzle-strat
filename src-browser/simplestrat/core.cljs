(ns simplestrat.core
  (:require [clojure.browser.dom :as dom]
            [clojure.browser.repl :as repl]
            [simplestrat.preloader :as preloader]
            [simplestrat.renderstate :as renderer]
            [simplestrat.gameworld :as world]
            [simplestrat.gameassets]
            [simplestrat.action :as action]
            ))



;; the current game state (map, characters) as an atom
(def current-gamestate (atom {}))

#_(defn newcharacterselected [character]
  ;; calculate allowed moves for this character
  (let [gamestate @current-gamestate
        movelocations (seqof-movesforcharacter gamestate character)
        attacklocations (seqof-attacksforcharacter gamestate character)]
    ;;(js/console.log (clj->js movelocations))
    (renderer/rebuildoverlay gamestate movelocations attacklocations)
    ;; need to intercept mouse clicks and check for move/attack clicks
    (renderer/redraw))
  )


(defn makestartingcharacters []
  [
   (world/create-character
    {:charactername "bob" :id 1 :iconindex 128 :coords [5 2] :team :team2 :starthealth 2 :actions [(action/createmoveaction "walk" 1 1)]})
   (world/create-character
    {:charactername "tom" :id 2 :iconindex 130 :coords [6 5] :team :team1 :starthealth 2 :actions [(action/createmoveaction "walk" 1 1)]})
   (world/create-character
    {:charactername "shemp" :id 3 :iconindex 191 :coords [8 5] :team :team1 :starthealth 2 :actions [(action/createmoveaction "run" 26 2)]})
   ])

(defn addstartingcharacters [gamestate]
  (let [characters (makestartingcharacters)]
    (reduce world/put-character gamestate characters)))

(defn makestartingstate []
  (let [gamemap (world/makestartingmap {:width 10 :height 10})
        emptygamestate (world/makeemptygamestate)]
    (-> emptygamestate
        (assoc :map gamemap)
        (addstartingcharacters)
        (world/advanceturn) ; so team1 basically starts first
        )))

(defn initializeboard []
  (let [startstate (makestartingstate)]
    (reset! current-gamestate startstate)
    ))

(defn startgame [loadedassets]
  (set! simplestrat.gameassets/assets loadedassets)
  #_(js/console.log loadedassets)
  (initializeboard)
  (renderer/initializeplayarea)
  (renderer/updategamestate! @current-gamestate)
  (let [characterexamine (world/get-character @current-gamestate 1)
        moveactions (action/seqof-charactermoveactions characterexamine)
        movelocations (action/seqof-movelocationsforcharacter @current-gamestate characterexamine nil)
        ]
    (js/console.log (clj->js characterexamine))
    (js/console.log (clj->js moveactions))
    (js/console.log (clj->js movelocations))))


;;
;; initialization
;;

(defn createjsstartgame []
  #_(repl/connect "http://localhost:9000/repl")
  (renderer/initializerenderer "target")
  
  ;; the preloader loads all the assets and then calls startgame
  ;; with the results
  (preloader/preloadgame simplestrat.gameassets/manifest startgame))

(set! (.-onload js/window) createjsstartgame)
;;(set! (.-onload js/window) craftystartgame)


