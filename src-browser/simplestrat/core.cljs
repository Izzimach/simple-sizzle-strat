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
    {:charactername "Angry Red Monster" :id 1 :iconindex 128 :coords [5 4] :team :team2 :starthealth 5 
     :actions [(action/createmoveaction "waddle" 100 27 1 "Waddle: Moves one space" :simplemove) 
               (action/createmajoraction "bite" 101 173 1 2 "Bite: damages an adjacent enemy" :singletarget) 
               #_(action/createmajoraction "burn" 102 56 1 1 "Burn: damages all adjacent enemies" :closeburst)]})
   #_(world/create-character
    {:charactername "Gold Demon" :id 2 :iconindex 131 :coords [7 4] :team :team2 :starthealth 3 
     :actions [(action/createmoveaction "crawl" 110 27 1 "Crawl: move one space" :simplemove) 
               (action/createmajoraction "spew" 111 141 3 1 "Spew: damages an enemy within three spaces" :singletarget)]})
   (world/create-character
    {:charactername "Orange Golem" :id 3 :iconindex 129 :coords [5 5] :team :team2 :starthealth 6
     :actions [(action/createmoveaction "stomp" 120 27 1 "Stomp: move one space" :simplemove) 
               (action/createmajoraction "punch" 121 69 1 2 "Punch: damages an adjacent enemy" :singletarget)
               (action/createmajoraction "rock" 122 33 3 1 "Hurl Rock: throws rock up to four spaces, damaging the target and adjacent enemies" :burst1)]})
   
   (world/create-character
    {:charactername "Knight" :id 4 :iconindex 160 :coords [3 3] :team :team1 :starthealth 6 
     :actions [(action/createmoveaction "walk" 200 27 1 "Walk: move one space"  :simplemove) 
               (action/createmajoraction "sword" 201 67 1 2 "Sword: damages an adjacent enemy" :singletarget)]})
   (world/create-character
    {:charactername "Guy with Gun" :id 5 :iconindex 191 :coords [2 4] :team :team1 :starthealth 3 
     :actions [(action/createmoveaction "walk" 210 27 1 "Walk: move one space" :simplemove) 
               (action/createmajoraction "shoot" 211 197 4 1 "Shoot: damage an enemy within four spaces" :singletarget)]})
   (world/create-character
    {:charactername "A Wizard Did It" :id 6 :iconindex 162 :coords [3 5] :team :team1 :starthealth 3
     :actions [(action/createmoveaction "walk" 220 27 1 "Walk: move one space" :simplemove) 
               #_(action/createmajoraction "flame" 221 56 2 2 "Flame Jet: attacks an enemy within two spaces")
               (action/createmajoraction "lightning" 222 60 3 1 "Lightning: attacks an enemy within three spaces, also damaging adjacent enemies" :burst1)]})
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

(defn restartgame []
  (initializeboard)
  (renderer/initializeplayarea)
  (renderer/updategamestate! @current-gamestate))

(defn changegamestate [changefunc]
  (js/console.log "Changegamestate called")
  (swap! current-gamestate changefunc)
  (renderer/updategamestate! @current-gamestate)
  @current-gamestate)


(defn startgame [loadedassets]
  (set! simplestrat.gameassets/assets loadedassets)
  #_(js/console.log loadedassets)
  #_(renderer/initializeplayarea)
  (renderer/setactionhook! changegamestate)
  (renderer/setrestarthook! restartgame)
  #_(initializeboard)
  #_(renderer/updategamestate! @current-gamestate)
  (restartgame)
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


