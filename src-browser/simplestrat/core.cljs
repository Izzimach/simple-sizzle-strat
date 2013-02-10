(ns simplestrat.core
  (:require [clojure.browser.dom :as dom]
            [clojure.browser.repl :as repl]
            [simplestrat.preloader :as preloader]))


;;(repl/connect "http://localhost:9000/repl")

(def spritescale 2)
(def basetilesize 12)
(def tilespacing (* spritescale basetilesize))

;; gameassets: images, sounds, etc. use (get gameassets "image") to
;; get images
(def gameassets {})

;; the current game state as an atom
(def gamestate (atom {:map [] :characters [] }))

;; contains various rendering objects. These are easelJS containers
(def renderer (atom {:stage nil :stage-map nil :stage-characters nil}))

;;
;; sprite sheet cache
;;

(def spritecache (atom {}))

(defn getspritesheet [spritesheetname]
  (if (contains? @spritecache spritesheetname)
    ;; spritesheet is already in the cache, just return it
    (get @spritecache spritesheetname)
    (let [spritesheetimage (get-in gameassets ["image" spritesheetname])
          spritesheet-config (clj->js {:images [spritesheetimage] :frames {:width 12 :height 12}})
          spritesheet (createjs/SpriteSheet. spritesheet-config)]
      ;; add the new spritesheet to the cache and return it
      (swap! spritecache assoc spritesheetname spritesheet)
      spritesheet)))


#_(defn clearscreen [gamescreen]
  (let [width (-> gamescreen .-canvas .-width)
        height (-> gamescreen .-canvas .-width)]
    (aset gamescreen "fillStyle" "#000000")
    (.fillRect gamescreen 0 0 width height)))

#_(defn simpletitle [gamescreen]
  (let [canvas (.-canvas gamescreen)
        width (.-width canvas)
        height (.-height canvas)
        textx (/ width 2)
        texty (/ height 2)]
    (aset gamescreen "fillStyle" "orange")
    (.fillText gamescreen "argh!" textx texty)))

#_(defn addimage [stage img]
  (let [bitmap (createjs/Bitmap. img)]
    (.addChild stage bitmap)
    (aset bitmap "x" 400)
    (aset bitmap "y" 200)
    (.update stage)))


(defn updatecharactersprite [character]
  ;; create sprite if needed
  (let [charactersprite (:sprite character)
        newx (* tilespacing (:x character))
        newy (* tilespacing (:y character))]
    (aset charactersprite "x" newx)
    (aset charactersprite "y" newy)))

(defn addcharactersprite [character]
  (let [freshsprite (getspritesheet ["icons"])
        characterstage (:stage-characters @renderer)]
    (.addChild characterstage freshsprite)
    (assoc character :sprite freshsprite)))

(defn removecharactersprite [character]
  (let [characterstage (:stage-characters @renderer)]
    (.removeChild characterstage (:sprite character))
    (dissoc character :sprite)))

(defn action-addcharacter [gamestate freshcharacter]
  (conj gamestate freshcharacter))

(defn action-movecharacter [gamestate character newx newy]
  (let [characters (:characters gamestate)
        movedcharacter (-> character (assoc :x newx) (assoc :y newy))
        modifiedcharacters (conj (disj characters character) movedcharacter)]
    (assoc gamestate :characters modifiedcharacters)))

(defn action-removecharacter [gamestate character]
  (let [modifiedcharacters (disj (:characters gamestate) character)]
    (assoc gamestate :characters modifiedcharacters)))

(defn makecharacters []
  
  )


(defn chooseterrain [x y]
  (if (> x y)
    39
    54))

(defn generatemap [{:keys [width height]}]
  (for [x (range 0 width)
        y (range 0 height)]
    {:x x :y y :terrain (chooseterrain x y)}))

(defn rendermap [gamemap]
  (let [gameboardcontainer (:stage-map @renderer)
        spritesheet (getspritesheet "icons")]
    (.removeAllChildren gameboardcontainer)
    (doseq [tile gamemap
            :let [{:keys [x y terrain]} tile
                  freshtile (createjs/BitmapAnimation. spritesheet)]]
      (aset freshtile "x" (* x tilespacing))
      (aset freshtile "y" (* y tilespacing))
      (aset freshtile "scaleX" spritescale)
      (aset freshtile "scaleY" spritescale)
      (.gotoAndStop freshtile terrain)
      (.addChild gameboardcontainer freshtile))))

(defn setupboard []
  ;; should have the gamestate setup by now
  (let [gamemap (generatemap {:width 10 :height 10})]
    ;; generate a bunch of icons based off of the map data
    #_(js/console.log mapicons)
    (swap! gamestate assoc :map gamemap)
    (rendermap gamemap)
    (.update (:stage @renderer))))


(defn createnamedcontainer [name]
  (let [freshcontainer (createjs/Container.)]
    (aset freshcontainer "id" name)
    freshcontainer))

(defn startgame [loadedassets]
  (set! gameassets loadedassets)
  #_(js/console.log loadedassets)
  (let [stage (:stage @renderer)
        tilemap (createnamedcontainer "tilemap")
        characters (createnamedcontainer "characters")]
    (.addChild stage tilemap)
    (aset tilemap "id" "tilemap")
    (aset tilemap "x" 100)
    (aset tilemap "y" 100)
    (.addChild stage characters)
    (aset characters "id" "characters")
    (aset characters "x" 100)
    (aset characters "y" 100)
    ;; embed direct references to the map and character containers for
    ;; easy manipulation later
    (swap! renderer assoc :stage-map tilemap)
    (swap! renderer assoc :stage-characters characters))
  (setupboard))

(def manifest [{:id "icons" :src "gfx/placeholders_tigsource.png"}])

;;
;; initialization
;;

(defn createjsstartgame []
  (let [canvas (dom/get-element "target")
        stage (createjs/Stage. canvas)]
    (swap! renderer assoc :stage stage)
    ;; the preloader loads all the assets and then calls startgame with the results
    (preloader/preloadgame stage manifest startgame)))

(set! (.-onload js/window) createjsstartgame)
;;(set! (.-onload js/window) craftystartgame)


