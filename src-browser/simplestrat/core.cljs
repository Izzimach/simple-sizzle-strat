(ns simplestrat.core
  (:require [clojure.browser.dom :as dom]
            [clojure.browser.repl :as repl]))


;;(repl/connect "http://localhost:9000/repl")

;; An instance of an easeljs Stage
(def gamestage)

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
    :grass
    :dirt))

(defn generatemap [{:keys [width height]}]
  (for [x (range 0 width)
        y (range 0 height)]
    {:x x :y y :terrain (chooseterrain x y)}))

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
    (aset text "rotation" 20)))

(defn processassets [assets]
  "Given a set of assets, sorts out the assets by type. Produces a map with a key for each asset type (image, sound, etc.). Values are maps from id to resource."
  (let []
    ;; each assets has an id, type, and result (data)
    (js/console.log (clj->js assets))
    (reduce #(assoc-in %1 [(-> %2 .-item .-type) (-> %2 .-item .-id)] (.-result %2))
            {}
            assets)))

(defn preloadcomplete [stage assets]
  (let [processed (processassets assets)
        images (get processed "image")]
;;    (js/console.log (clj->js processed))
;;    (js/console.log (clj->js (keys processed)))
;;    (js/console.log (clj->js images))
    (addimage stage (get images "icons"))))

;;
;; preloader - loading the assets
;;

(def manifest [{:src  "gfx/placeholders_tigsource.png" :id "icons"}])

(defn preloadgame [stage]
  (let [jsmanifest (clj->js manifest)
        loader (createjs/LoadQueue.)
        ;; loadedassets really doesn't need to be an atom since JS is
        ;; single-threaded, but oh well.
        loadedassets (atom [])
        accumulateassets #(swap! loadedassets conj %1)]
    (.addEventListener loader "complete" #(preloadcomplete stage @loadedassets))
    (.addEventListener loader "fileload" accumulateassets)
    (.loadManifest loader jsmanifest)))

;;
;; initialization
;;

(defn createjsstartgame []
  (let [canvas (dom/get-element "target")
        stage (createjs/Stage. canvas)]
    (set! gamestage stage)
    (swap! gamestate assoc :stage stage)
    (preloadgame stage)))

(set! (.-onload js/window) createjsstartgame)
;;(set! (.-onload js/window) craftystartgame)


