(ns simplestrat.core
  (:use [clojure.set :only (difference)])
  (:require [clojure.browser.dom :as dom]
            [clojure.browser.repl :as repl]
            [simplestrat.preloader :as preloader]
            ))


;;(repl/connect "http://localhost:9000/repl")

(def spritescale 2)
(def basetilesize 12)
(def tilespacing (* spritescale basetilesize))
(def manifest [{:id "icons" :src "gfx/placeholders_tigsource.png"}])

;; the current game state as an atom
(def current-gamestate (atom {:map [] :characters [] }))


;; gameassets: images, sounds, etc. use (get gameassets "image") to
;; get images
(def gameassets {})

;;
;; current renderstate, contains various rendering objects. These are
;; easelJS containers
;;

(def displayed-renderstate (atom {:stage nil :stage-map nil :stage-characters nil}))

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

(defn displaycharactersprite [renderstate character]
  ;; create sprite if needed
  (let [spritemap (:spritemap renderstate)
        charactersprite (get spritemap (:iconindex character))
        newx (* tilespacing (:x character))
        newy (* tilespacing (:y character))
        characterstage (:stage-characters renderstate)]
    (aset charactersprite "x" newx)
    (aset charactersprite "y" newy)
    (.addChild characterstage charactersprite)))


(defn addcharactersprite [spritemap iconindex]
  (let [freshbitmap (createjs/BitmapAnimation. (getspritesheet "icons"))]
    ;;(js/console.log iconindex)
    (.gotoAndStop freshbitmap iconindex)
    (aset freshbitmap "scaleX" spritescale)
    (aset freshbitmap "scaleY" spritescale)
    (assoc spritemap iconindex freshbitmap))) 

(defn removecharactersprite [spritemap iconindex]
  ;; just dissoc and let the GC collect it, I guess
  (dissoc spritemap iconindex))

(defn generatemissingsprites [spritemap missingsprites]
  "Looks for characters that do not have a sprite associated with them, creates any missing sprites,
and returns an updated sprite map which should have all the needed sprites."
  (reduce addcharactersprite spritemap (seq missingsprites)))

(defn removeunusedsprites [spritemap unusedsprites]
  (reduce removecharactersprite spritemap (seq  unusedsprites)))

(defn syncspritemaptocharacters [spritemap characters]
  "Removes any sprites that do not have a character using them"
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

(defn updatecharacterdisplay [gamestate renderstateatom]
  (let [characters (:characters gamestate)
        spritemap (:spritemap @renderstateatom)
        syncedspritemap (syncspritemaptocharacters spritemap characters)]
    ;; mutating step to add any missing sprites
    (swap! renderstateatom assoc :spritemap syncedspritemap)
    ;; mutating step to remove all sprites currently displayed and rebuild the display
    ;; list from the gamestate
    (let [renderstate @renderstateatom
          stage-characters (:stage-characters renderstate)]
      (.removeAllChildren stage-characters)
      (doseq [character characters]
        (displaycharactersprite renderstate character)))))

;; simple state updates: add, remove, and move a character
(defn createcharacter [charactername iconindex x y]
  {:name charactername :iconindex iconindex :x x :y y})

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

(defn makecharacters [gamestate]
  (let [character1 (createcharacter "bob" 128 5 2)
        character2 (createcharacter "tom" 130 6 5)
        character3 (createcharacter "shemp" 191 8 5)]
    (-> gamestate
        (update-addcharacter character1)
        (update-addcharacter character2)
        (update-addcharacter character3))))

(defn chooseterrain [x y]
  (if (> x y)
    39
    54))

(defn generatemap [{:keys [width height]}]
  (for [x (range 0 width)
        y (range 0 height)]
    {:x x :y y :terrain (chooseterrain x y)}))

(defn rebuildmapdisplaylist [gamestate renderstate]
  (let [gamemap (:map gamestate)
        gameboardcontainer (:stage-map renderstate)
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
    (swap! current-gamestate assoc :map gamemap)
    (swap! current-gamestate makecharacters)
    (rebuildmapdisplaylist @current-gamestate @displayed-renderstate )
    ;; the displayed-renderstate is not deref-ed here since
    ;; rebuilding the character display list may require
    ;; modifying the renderstate to add/remove sprites
    (updatecharacterdisplay @current-gamestate displayed-renderstate)
    (.update (:stage @displayed-renderstate))))

(defn createeaseljscontainer [name]
  (let [freshcontainer (createjs/Container.)]
    (aset freshcontainer "id" name)
    freshcontainer))

(defn startgame [loadedassets]
  (set! gameassets loadedassets)
  #_(js/console.log loadedassets)
  (let [stage (:stage @displayed-renderstate)
        tilemap (createeaseljscontainer "tilemap")
        characters (createeaseljscontainer "characters")]
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
    (swap! displayed-renderstate assoc :stage-characters characters))
  (setupboard))


;;
;; initialization
;;

(defn createjsstartgame []
  (let [canvas (dom/get-element "target")
        stage (createjs/Stage. canvas)]
    (swap! displayed-renderstate assoc :stage stage)
    ;; the preloader loads all the assets and then calls startgame with the results
    (preloader/preloadgame stage manifest startgame)))

(set! (.-onload js/window) createjsstartgame)
;;(set! (.-onload js/window) craftystartgame)


