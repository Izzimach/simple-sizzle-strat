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

(defn- getimageasset [imagename]
  (get-in gameassets/assets ["image" imagename]))

;;
;; sprite sheet cache
;;

(def spritecache (atom {}))

(defn- getspritesheet [spritesheetname]
  (if (contains? @spritecache spritesheetname)
    ;; spritesheet is already in the cache, just return it
    (get @spritecache spritesheetname)
    (let [spritesheetimage (getimageasset spritesheetname)
          spritesheet-config (clj->js {:images [spritesheetimage] :frames {:width 12 :height 12}})
          spritesheet (createjs/SpriteSheet. spritesheet-config)]
      ;; add the new spritesheet to the cache and return it
      (swap! spritecache assoc spritesheetname spritesheet)
      spritesheet)))

;;
;; basic sprite tile manipulation
;;

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

;;
;; character sprite functions
;;

(defn- displaycharactermapsprite [renderstate character]
  (let [spritemap (:mapsprites renderstate)
        charactersprite (get spritemap (:uniqueid character))
        newx (* tilespacing (:x character))
        newy (* tilespacing (:y character))
        characterstage (:stage-characters renderstate)]
    ;;(js/console.log (clj->js spritemap))
    ;;(js/console.log (clj->js character))
    (positionsprite charactersprite newx newy)
    (makestandardsizeandorigin charactersprite)
    (.addChild characterstage charactersprite)))

(defn- addcharactersprite [spritemap character]
  (let [freshbitmap (createjs/BitmapAnimation. (getspritesheet "icons"))
        standardscale spritescale
        hightlightedscale (* 1.2 spritescale)]
    ;;(js/console.log (clj->js character))
    ;; right now sprites are simple animations with one frame.
    (.gotoAndStop freshbitmap (:iconindex character))
    (makestandardsizeandorigin freshbitmap)
    ;; assign mouse handlers to expand this icon a bit when the mouse
    ;; hovers over it
    (aset freshbitmap "mouseEnabled" true)
    (aset freshbitmap "onMouseOver" (fn [_] (scalesprite freshbitmap 1.2) (redraw)))
    (aset freshbitmap "onMouseOut" (fn [_] (makestandardsizeandorigin freshbitmap) (redraw)))
    (assoc spritemap (:uniqueid character) freshbitmap))) 

(defn- removecharactersprite [spritemap character]
  ;; just dissoc and let the GC collect it, I guess
  (dissoc spritemap (:uniqueid character)))

(defn- generatemissingsprites [spritemap missingcharacters]
  "Looks for characters that do not have a sprite associated with them, creates any missing sprites,
and returns an updated sprite map which should have all the needed sprites."
  (reduce addcharactersprite spritemap (seq missingcharacters)))

(defn- removeunusedsprites [spritemap unusedspriteids]
  (reduce removecharactersprite spritemap (seq unusedspriteids)))

(defn- syncspritemaptocharacters [spritemap characterseq]
  "Adds sprites for charaters that do not yet have them, and removes any sprites that do not have a character using them. Returns the updated spritemap."
  (let [characteruniqueids (set (map :uniqueid characterseq))
        spriteuniqueids (set (keys spritemap))
        unusedspriteids (difference spriteuniqueids characteruniqueids)
        missingspriteids (difference characteruniqueids spriteuniqueids)]
    ;; now add the new sprites and remove the old ones
    ;;(js/console.log (clj->js spritemap))
    ;;(js/console.log (clj->js missingspriteids))
    ;;(js/console.log (clj->js unusedspriteids))
    ;;(js/console.log (clj->js (rest characterseq)))
    (-> spritemap
        (generatemissingsprites characterseq)
        (removeunusedsprites unusedspriteids))))

(defn- syncsprites! [spritemapkeyword characterseq]
  "Updates the sprites used for the given spritemap. Returns the possibly-updated spritemap."
  (let [;; if spritemap is nil, defaults to {}
        spritemap (or (spritemapkeyword @displayed-renderstate) {})
        syncedspritemap (syncspritemaptocharacters spritemap characterseq)]
    ;; swap if something changed
    (if (= spritemap syncedspritemap)
      nil
      (swap! displayed-renderstate assoc spritemapkeyword syncedspritemap))
    syncedspritemap))

(defn rebuildcharacterdisplaylist [gamestate]
  (let [characters (:characters gamestate)]
    ;; mutating step to add missing sprites and remove unused sprites
    (syncsprites! :mapsprites (vals characters))
    (let [renderstate @displayed-renderstate
          stage-characters (:stage-characters renderstate)]
      (.removeAllChildren stage-characters)
      (doseq [character (vals characters)]
        (displaycharactermapsprite renderstate character)))))

;;
;; map sprite function(s)
;;

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

;;
;; left/right GUI functions
;;

(defn- displaycharacterrostersprite [rostercontainer rosterspritemap character x y]
  (let [charactersprite (get rosterspritemap (:uniqueid character))]
    (positionsprite charactersprite x y)
    (makestandardsizeandorigin charactersprite)
    (scalesprite charactersprite 1.8)
    (.addChild rostercontainer charactersprite)))

(def ^:private teamkeywordmap {1 :team1rostersprites 2 :team2rostersprites})

(defn rebuildrosterdisplaylist [gamestate rosterteam]
  (let [rosterkeyword (get teamkeywordmap rosterteam)
        rostercontainer (get (:rostermap @displayed-renderstate) rosterteam)
        allcharacters (vals (:characters gamestate))
        teamcharacters (filter #(= (:team %1) rosterteam) allcharacters)
        orderedteamcharacters (into-array (sort-by :uniqueid teamcharacters))
        rostersize (count teamcharacters)]
    (.removeAllChildren rostercontainer)
    ;;(js/console.log rosterkeyword)
    ;;(js/console.log teamcharacters)
    (syncsprites! rosterkeyword teamcharacters)
    (let [rosterspritemap (get @displayed-renderstate rosterkeyword)]
      (doseq [characterindex (range rostersize)
              :let [character (get orderedteamcharacters characterindex)
                    x 0
                    y (* 2 tilespacing characterindex)]]
        (displaycharacterrostersprite rostercontainer rosterspritemap character x y)))))


;;
;; initialization functions
;;

(defn- createeaseljscontainer [name x y]
  (let [freshcontainer (createjs/Container.)]
    (aset freshcontainer "id" name)
    (aset freshcontainer "name" name)
    (aset freshcontainer "x" x)
    (aset freshcontainer "y" y)
    (aset freshcontainer "mouseEnabled" true)
    ;;(aset freshcontainer "onClick" (fn [_] (js/console.log "clicked")))
    freshcontainer))

(defn- createcharacterroster [name x y]
  (let [rostercontainer (createeaseljscontainer name x y)
        background (createjs/Bitmap. (getimageasset "GUIbackground"))]
    (.addChild rostercontainer background)
    rostercontainer
    ))

(defn initializeplayarea []
  (let [stage (:stage @displayed-renderstate)
        tilemap (createeaseljscontainer "tilemap" 200 100)
        characters (createeaseljscontainer "characters" 200 100)
        leftRoster (createcharacterroster "team1Roster" 50 100)
        rightRoster (createcharacterroster "team2Roster" 500 100)
        rostermap {1 leftRoster 2 rightRoster}]
    (.removeAllChildren stage)
    (.addChild stage tilemap)
    (.addChild stage characters)
    (.addChild stage leftRoster)
    (.addChild stage rightRoster)
    ;; embed direct references to the map and character containers for
    ;; easy manipulation later
    (swap! displayed-renderstate assoc :stage-map tilemap :stage-characters characters)
    (swap! displayed-renderstate assoc :leftRoster leftRoster :rightRoster rightRoster :rostermap rostermap)))

(defn initializerenderer [canvasname]
  (let [canvas (dom/get-element canvasname)
        stage (createjs/Stage. canvas)]
    ;; enable mouse over for tooltips, hover, etc.
    (.enableMouseOver stage 10)
    (swap! displayed-renderstate assoc :stage stage)))
