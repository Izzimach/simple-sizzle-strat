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
    {:charactername "Angry Red Monster" :id 1 :iconindex 128 :coords [5 4] :team :team2 :starthealth 2 
     :actions [(action/createmoveaction "waddle" 1 1) (action/createmajoraction "bite" 1 1 1) 
               (action/createmajoraction "flame" 1 1 2)]})
   (world/create-character
    {:charactername "Gold Demon" :id 2 :iconindex 131 :coords [7 4] :team :team2 :starthealth 3 
     :actions [(action/createmoveaction "crawl" 1 1) (action/createmajoraction "spew" 1 1 1)]})
   (world/create-character
    {:charactername "Orange Golem" :id 3 :iconindex 129 :coords [5 6] :team :team2 :starthealth 5 
     :actions [(action/createmoveaction "stomp" 1 1) (action/createmajoraction "punch" 1 1 1)]})
   
   (world/create-character
    {:charactername "Knight" :id 4 :iconindex 160 :coords [3 3] :team :team1 :starthealth 5 
     :actions [(action/createmoveaction "walk" 1 1) (action/createmajoraction "sword" 1 1 1)]})
   (world/create-character
    {:charactername "Guy with Gun" :id 5 :iconindex 191 :coords [2 4] :team :team1 :starthealth 2 
     :actions [(action/createmoveaction "walk" 26 1) (action/createmajoraction "shoot" 1 3 1)]})
   (world/create-character
    {:charactername "A Wizard Did It" :id 6 :iconindex 162 :coords [3 5] :team :team1 :starthealth 2 
     :actions [(action/createmoveaction "walk" 1 1) (action/createmajoraction "lightning" 1 1 1)]})
   ])

(defn addstartingcharacters [gamestate]
  (let [characters (makestartingcharacters)]
    (reduce world/put-character gamestate characters)))

(defn makestartingstate []
  (let [gamemap (world/makestartingmap {:width 8 :height 8})
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
  #_(repl/connect "http://localhost:9000/repl")
  (renderer/initializerenderer "target")
  
  ;; the preloader loads all the assets and then calls startgame
  ;; with the results
  (preloader/preloadgame simplestrat.gameassets/manifest startgame))

(set! (.-onload js/window) createjsstartgame)
;;(set! (.-onload js/window) craftystartgame)


