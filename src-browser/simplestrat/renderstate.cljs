(ns simplestrat.renderstate
  (:use [clojure.set :only (difference)])
  (:require [simplestrat.gameassets :as gameassets]
            [clojure.browser.dom :as dom]))


(def spritescale 2)
(def basetilesize 12)
(def standardtilesize (* spritescale basetilesize))
(def tilespacing standardtilesize)


;;
;; current renderstate, contains various rendering objects. These are
;; easelJS containers
;;

(def displayed-renderstate (atom {:stage nil :stage-map nil :stage-characters nil}))

(defn redraw [] (.update (:stage @displayed-renderstate)))

;;
;; sprite sheet cache
;;

(def spritecache (atom {}))

(defn- getspritesheet [spritesheetname]
  (if (contains? @spritecache spritesheetname)
    ;; spritesheet is already in the cache, just return it
    (get @spritecache spritesheetname)
    (let [spritesheetimage (get-in gameassets/assets ["image" spritesheetname])
          spritesheet-config (clj->js {:images [spritesheetimage] :frames {:width 12 :height 12}})
          spritesheet (createjs/SpriteSheet. spritesheet-config)]
      ;; add the new spritesheet to the cache and return it
      (swap! spritecache assoc spritesheetname spritesheet)
      spritesheet)))


(defn- scalesprite [sprite scale]
  (let [normalizedscale (* scale spritescale)]
    (aset sprite "scaleX" normalizedscale)
    (aset sprite "scaleY" normalizedscale)))

(defn- positionsprite [sprite x y]
  (aset sprite "x" x)
  (aset sprite "y" y))

(defn- makestandardsizeandorigin [sprite]
  (let [halfsize (* 0.5 basetilesize) ; use unscaled bitmap size here!
        ]
    (aset sprite "regX" halfsize)
    (aset sprite "regY" halfsize)
    (scalesprite sprite 1.0)))

(defn- displaycharactersprite [renderstate character]
  ;; create sprite if needed
  (let [spritemap (:spritemap renderstate)
        charactersprite (get spritemap (:iconindex character))
        newx (* tilespacing (:x character))
        newy (* tilespacing (:y character))
        characterstage (:stage-characters renderstate)]
    (positionsprite charactersprite newx newy)
    (makestandardsizeandorigin charactersprite)
    (.addChild characterstage charactersprite)))

(defn- addcharactersprite [spritemap iconindex]
  (let [freshbitmap (createjs/BitmapAnimation. (getspritesheet "icons"))
        standardscale spritescale
        hightlightedscale (* 1.2 spritescale)]
    ;;(js/console.log iconindex)
    ;; right now sprites are simple animations with one frame.
    (.gotoAndStop freshbitmap iconindex)
    (makestandardsizeandorigin freshbitmap)
    (aset freshbitmap "mouseEnabled" true)
    ;; assign mouse handlers to expand this icon a bit when the mouse
    ;; hovers over it
    (aset freshbitmap "onMouseOver" (fn [_] (scalesprite freshbitmap 1.2) (redraw)))
    (aset freshbitmap "onMouseOut" (fn [_] (makestandardsizeandorigin freshbitmap) (redraw)))
    
    (assoc spritemap iconindex freshbitmap))) 

(defn- removecharactersprite [spritemap iconindex]
  ;; just dissoc and let the GC collect it, I guess
  (dissoc spritemap iconindex))

(defn- generatemissingsprites [spritemap missingsprites]
  "Looks for characters that do not have a sprite associated with them, creates any missing sprites,
and returns an updated sprite map which should have all the needed sprites."
  (reduce addcharactersprite spritemap (seq missingsprites)))

(defn- removeunusedsprites [spritemap unusedsprites]
  (reduce removecharactersprite spritemap (seq  unusedsprites)))

(defn- syncspritemaptocharacters [spritemap characters]
  "Adds sprites for charaters that do not yet have them, and removes any sprites that do not have a character using them"
  (let [charactericonindices (set (map #(:iconindex %1) characters))
        spriteiconindices (set (keys spritemap))
        unusedsprites (difference spriteiconindices charactericonindices)
        missingsprites (difference charactericonindices spriteiconindices)]
    ;; now add the new sprites and remove the old ones
    ;;(js/console.log (clj->js missingsprites))
    ;;(js/console.log (clj->js unusedsprites))
    (-> spritemap
        (generatemissingsprites missingsprites)
        (removeunusedsprites unusedsprites))))

(defn rebuildcharacterdisplaylist [gamestate]
  (let [characters (:characters gamestate)
        spritemap (:spritemap @displayed-renderstate)
        syncedspritemap (syncspritemaptocharacters spritemap characters)]
    ;; mutating step to add any missing sprites
    (swap! displayed-renderstate assoc :spritemap syncedspritemap)
    ;; mutating step to remove all sprites currently displayed and rebuild the display
    ;; list from the gamestate
    (let [renderstate @displayed-renderstate
          stage-characters (:stage-characters renderstate)]
      (.removeAllChildren stage-characters)
      (doseq [character characters]
        (displaycharactersprite renderstate character)))))

(defn rebuildmapdisplaylist [gamestate]
  (let [gamemap (:map gamestate)
        gameboardcontainer (:stage-map @displayed-renderstate)
        spritesheet (getspritesheet "icons")]
    (.removeAllChildren gameboardcontainer)
    (doseq [tile gamemap
            :let [{:keys [x y terrain]} tile
                  freshtile (createjs/BitmapAnimation. spritesheet)
                  tilex (* x tilespacing)
                  tiley (* y tilespacing)]]
      (positionsprite freshtile tilex tiley)
      (makestandardsizeandorigin freshtile)
      (.gotoAndStop freshtile terrain)
      (.addChild gameboardcontainer freshtile))))

(defn- createeaseljscontainer [name]
  (let [freshcontainer (createjs/Container.)]
    (aset freshcontainer "id" name)
    ;; disable mouse for the container. This ensures
    ;; that the children will receive mouse events, not the container
    ;; (which doesn't need them usually)
    (aset freshcontainer "mouseEnabled" true)
    ;;(aset freshcontainer "onClick" (fn [_] (js/console.log "clicked")))
    freshcontainer))

(defn initializeplayarea []
  (let [stage (:stage @displayed-renderstate)
        tilemap (createeaseljscontainer "tilemap")
        characters (createeaseljscontainer "characters")]
    (.removeAllChildren stage)
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
    (swap! displayed-renderstate assoc :stage-map tilemap)
    (swap! displayed-renderstate assoc :stage-characters characters)))

(defn initializerenderer [canvasname]
  (let [canvas (dom/get-element canvasname)
        stage (createjs/Stage. canvas)]
    ;; enable mouse over for tooltips, hover, etc.
    (.enableMouseOver stage 10)
    (swap! displayed-renderstate assoc :stage stage)))
