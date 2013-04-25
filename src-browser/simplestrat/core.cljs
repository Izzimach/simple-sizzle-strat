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
    {:charactername "Angry monster" :id 1 :iconindex 128 :coords [5 4] :team :team2 :starthealth 2 
     :actions [(action/createmoveaction "crawl" 1 1) (action/createmajoraction "bite" 1 1 1)]})
   (world/create-character
    {:charactername "Angry monster" :id 2 :iconindex 128 :coords [7 4] :team :team2 :starthealth 3 
     :actions [(action/createmoveaction "crawl" 1 1) (action/createmajoraction "spew" 1 1 1)]})
   (world/create-character
    {:charactername "Tom" :id 3 :iconindex 130 :coords [3 3] :team :team1 :starthealth 2 
     :actions [(action/createmoveaction "walk" 1 1) (action/createmajoraction "punch" 1 1 1)]})
   (world/create-character
    {:charactername "Shemp" :id 4 :iconindex 191 :coords [3 5] :team :team1 :starthealth 2 
     :actions [(action/createmoveaction "run" 26 2) (action/createmajoraction "shoot" 1 3 1)]})
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

(defn changegamestate [changefunc]
  (js/console.log "Changegamestate called")
  (swap! current-gamestate changefunc)
  (renderer/updategamestate! @current-gamestate))


(defn startgame [loadedassets]
  (set! simplestrat.gameassets/assets loadedassets)
  #_(js/console.log loadedassets)
  (initializeboard)
  (renderer/initializeplayarea)
  (renderer/setactionhook! changegamestate)
  (renderer/updategamestate! @current-gamestate)
  #_(let [characterexamine (world/get-character @current-gamestate 1)
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
  (repl/connect "http://localhost:9000/repl")
  (renderer/initializerenderer "target")
  
  ;; the preloader loads all the assets and then calls startgame
  ;; with the results
  (preloader/preloadgame simplestrat.gameassets/manifest startgame))

(set! (.-onload js/window) createjsstartgame)
;;(set! (.-onload js/window) craftystartgame)


