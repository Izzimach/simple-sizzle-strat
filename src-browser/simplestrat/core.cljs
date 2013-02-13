(ns simplestrat.core
  (:require [clojure.browser.dom :as dom]
            [clojure.browser.repl :as repl]
            [simplestrat.preloader :as preloader]
            [simplestrat.renderstate :as renderer]
            [simplestrat.gameassets]
            ))



;; the current game state as an atom
(def current-gamestate (atom {:map [] :characters [] }))

;;
;; character management
;;

(defn createcharacter [charactername iconindex x y]
  {:name charactername :iconindex iconindex :x x :y y :nextaction 1})

(defn update-addcharacter [gamestate freshcharacter]
  (let [modifiedcharacters (conj (:characters gamestate) freshcharacter)]
    (assoc gamestate :characters modifiedcharacters)))

(defn update-movecharacter [gamestate character newx newy]
  (let [characters (:characters gamestate)
        movedcharacter (-> character (assoc :x newx) (assoc :y newy))
        modifiedcharacters (conj (disj characters character) movedcharacter)]
    (assoc gamestate :characters modifiedcharacters)))

(defn update-removecharacter [gamestate character]
  (let [modifiedcharacters (disj (:characters gamestate) character)]
    (assoc gamestate :characters modifiedcharacters)))

(defn makestartingcharacters []
  [(createcharacter "bob" 128 5 2)
     (createcharacter "tom" 130 6 5)
     (createcharacter "shemp" 191 8 5)])

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

(defn setupboard []
  ;; should have the gamestate setup by now
  (let [gamemap (makestartingmap {:width 10 :height 10})
        characters (makestartingcharacters)]
    ;; generate a bunch of icons based off of the map data
    #_(js/console.log mapicons)
    (swap! current-gamestate assoc :map gamemap)
    (swap! current-gamestate (partial reduce update-addcharacter)  characters)
    (renderer/rebuildmapdisplaylist @current-gamestate)
    (renderer/rebuildcharacterdisplaylist @current-gamestate)
    (renderer/redraw)))

(defn startgame [loadedassets]
  (set! simplestrat.gameassets/assets loadedassets)
  #_(js/console.log loadedassets)
  (renderer/initializeplayarea)
  (setupboard))


;;
;; initialization
;;

(defn createjsstartgame []
  ;;(repl/connect "http://localhost:9000/repl")
  (renderer/initializerenderer "target")
  ;; the preloader loads all the assets and then calls startgame
  ;; with the results
  (preloader/preloadgame simplestrat.gameassets/manifest startgame))

(set! (.-onload js/window) createjsstartgame)
;;(set! (.-onload js/window) craftystartgame)


