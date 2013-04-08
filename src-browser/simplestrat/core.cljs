(ns simplestrat.core
  (:require [clojure.browser.dom :as dom]
            [clojure.browser.repl :as repl]
            [simplestrat.preloader :as preloader]
            [simplestrat.renderstate :as renderer]
            [simplestrat.gamestate :as gamestate]
            [simplestrat.gameassets]
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

(defn initializeboard []
  (let [gamestate (gamestate/makestartingstate)]
    (reset! current-gamestate gamestate)
    ))

(defn startgame [loadedassets]
  (set! simplestrat.gameassets/assets loadedassets)
  #_(js/console.log loadedassets)
  (initializeboard)
  (renderer/initializeplayarea)
  (renderer/updategamestate! @current-gamestate)
  (let [characterexamine (gamestate/get-character @current-gamestate 1)
        moveactions (gamestate/seqof-charactermoveactions characterexamine)
        movelocations (gamestate/seqof-movelocationsforcharacter @current-gamestate characterexamine nil)
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


