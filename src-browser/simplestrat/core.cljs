(ns simplestrat.core
  (:require [clojure.browser.dom :as dom]
            [clojure.browser.repl :as repl]
            [simplestrat.preloader :as preloader]))


;;(repl/connect "http://localhost:9000/repl")

;; gameassets
(def gameassets)

;; game state as an atom
(def gamestate (atom {:stage nil :map nil :characters nil }))

(defn clearscreen [gamescreen]
  (let [width (-> gamescreen .-canvas .-width)
        height (-> gamescreen .-canvas .-width)]
    (aset gamescreen "fillStyle" "#000000")
    (.fillRect gamescreen 0 0 width height)))

(defn simpletitle [gamescreen]
  (let [canvas (.-canvas gamescreen)
        width (.-width canvas)
        height (.-height canvas)
        textx (/ width 2)
        texty (/ height 2)]
    (aset gamescreen "fillStyle" "orange")
    (.fillText gamescreen "argh!" textx texty)))

(defn addimage [stage img]
  (let [bitmap (createjs/Bitmap. img)]
    (.addChild stage bitmap)
    (aset bitmap "x" 400)
    (aset bitmap "y" 200)
    (.update stage)))

(defn putonstage [stage shape x y]
  (.addChild stage shape)
  (aset shape "x" x)
  (aset shape "y" y)
  stage)

(defn chooseterrain [x y]
  (if (> x y)
    0
    3))

(defn generatemap [{:keys [width height]}]
  (for [x (range 0 width)
        y (range 0 height)]
    {:x x :y y :terrain (chooseterrain x y)}))

(comment
  (defn initeaselold [canvas]
  (let [shape (createjs/Shape.)
        shape-graphics (aget shape "graphics")
        stage (createjs/Stage. canvas)
        text (createjs/Text. "text on the canvas" "36px Arial" "#0FF")
        checkX (js/Image.)
        ]
    (set! gamestage stage)
    (-> shape-graphics
        (.beginFill "blue")
        (.drawRoundRect 0 0 120 120 10))
    (-> stage
        (putonstage shape 100 100)
        (putonstage text 50 20))
    (aset text "rotation" 20))))

(defn setupboard []
  ;; should have the gamestate setup by now
  (let [scale 2
        tilespacing (* scale 12)
        stage (:stage @gamestate)
        gamemap (generatemap {:width 10 :height 10})
        mapicons (get-in gameassets ["image" "icons"])
        spritesheet-config (clj->js {:images [mapicons] :frames {:width 12 :height 12}})
        spritesheet (createjs/SpriteSheet. spritesheet-config)
        bitmapbase (createjs/BitmapAnimation. spritesheet)]
    ;; generate a bunch of icons based off of the map data
    #_(js/console.log mapicons)
    (doseq [tile gamemap
            :let [{:keys [x y terrain]} tile
                  freshtile (.clone bitmapbase)]]
      (aset freshtile "x" (* x tilespacing))
      (aset freshtile "y" (* y tilespacing))
      (aset freshtile "scaleX" scale)
      (aset freshtile "scaleY" scale)
      (.gotoAndStop freshtile terrain)
      (.addChild stage freshtile)
      )
    (.update stage)))

(defn startgame [loadedassets]
  (set! gameassets loadedassets)
  #_(js/console.log loadedassets)
  (setupboard))

(def manifest [{:id "icons" :src "gfx/placeholders_tigsource.png"}])

;;
;; initialization
;;

(defn createjsstartgame []
  (let [canvas (dom/get-element "target")
        stage (createjs/Stage. canvas)]
    (swap! gamestate assoc :stage stage)
    ;; the preloader loads all the assets and then calls startgame with the results
    (preloader/preloadgame stage manifest startgame)))

(set! (.-onload js/window) createjsstartgame)
;;(set! (.-onload js/window) craftystartgame)


