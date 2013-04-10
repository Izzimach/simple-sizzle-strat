(ns simplestrat.renderstate
  (:use [clojure.set :only (difference)])
  (:require [simplestrat.gameassets :as gameassets]
            [simplestrat.gameworld :as gameworld]
            [clojure.browser.dom :as dom]))


(def ^:const ^:private spritescale 2)
(def ^:const ^:private basetilesize 12)
(def ^:const ^:private standardtilesize (* spritescale basetilesize))
(def ^:const ^:private tilespacing standardtilesize)
(def ^:const ^:private rostertilescale 1.8) ; relative to standardtilesize
(def ^:const ^:private rostertilesize (* rostertilescale standardtilesize))

(def ^:const ^:private mapoffsetpixels [200 100])

(def ^:private not-nil? (comp not nil?))

;;
;;
;; current renderstate, contains various rendering objects. These are
;; easelJS containers
;;
;;

(def displayed-renderstate (atom {:gamestate nil :stage nil :stage-map nil :stage-characters nil :sprites {}}))

(defn redraw [renderstate]
  "Redraws graphics. doesn't modify the renderstate"
  (.update (:stage renderstate)))

(defn- getimageasset [imagename]
  (get-in gameassets/assets ["image" imagename]))

;;
;;
;; sprite sheet cache
;;
;;

(def spritecache (atom {}))

(defn- getspritesheet! [spritesheetname]
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
;;
;; basic sprite tile manipulation
;;
;;

(defn- scalesprite [sprite scale]
  (let [normalizedscale (* scale spritescale)]
    (doto sprite 
      (aset "scaleX" normalizedscale)
      (aset "scaleY" normalizedscale))))

(defn- positionsprite [sprite x y]
  (doto sprite 
    (aset "x" x)
    (aset "y" y)))

(defn- makestandardsizeandorigin [sprite]
  (let [halfsize (* 0.5 basetilesize) ; use unscaled bitmap size here!
        ]
    (doto sprite 
      (aset "regX" halfsize)
      (aset "regY" halfsize)
      (scalesprite 1.0))))

;;
;; mouse hovering and clicking, and character selection
;;

(declare highlightcharacteronmap
         unhighlightcharacteronmap
         highlightcharacterinroster
         unhighlightcharacterinroster
         findspriteforcharacter
         rebuildoverlay)

(defn- highlightcharacter [character]
  "Highlights current character. Doesn't modify the clojure-side renderstate"
  (let [renderstate @displayed-renderstate]
    (doto renderstate
      (highlightcharacteronmap character)
      (highlightcharacterinroster character)
      (redraw))))

(defn- unhighlightcharacter [character]
  (let [renderstate @displayed-renderstate]
    (doto renderstate
      (unhighlightcharacteronmap character)
      (unhighlightcharacterinroster character)
      (redraw))))

(defn- selectcharacter [character]
  (let [uniqueid (:uniqueid character)
        defaultaction (gameworld/getdefaultaction character)
        renderstate (assoc @displayed-renderstate :selectedcharacterid uniqueid :selectedcharacteraction defaultaction)
        gamestate (:gamestate renderstate)
        overlaydata (gameworld/getclickablesfor gamestate character defaultaction)
        newrenderstate (rebuildoverlay renderstate overlaydata)]
    (reset! displayed-renderstate newrenderstate)
    (redraw newrenderstate)))

(defn- selectcharacteraction [character active-action]
  )

;;
;;
;; character sprite functions
;;
;;
;; current sprite lists [:mapsprites :team1 :team2]

(defn- displaycharactermapsprite [renderstate character]
  (let [sprites (get-in renderstate [:sprites :mapsprites])
        charactersprite (get sprites (:uniqueid character))
        newx (* tilespacing (:x character))
        newy (* tilespacing (:y character))
        characterstage (:stage-characters renderstate)]
    (positionsprite charactersprite newx newy)
    (makestandardsizeandorigin charactersprite)
    (.addChild characterstage charactersprite)))

(defn- addcharactersprite [spritemap character]
  (let [freshbitmap (createjs/BitmapAnimation. (getspritesheet! "icons"))
        standardscale spritescale
        hightlightedscale (* 1.2 spritescale)]
    ;;(js/console.log (clj->js character))
    ;; right now sprites are simple animations with one frame.
    (.gotoAndStop freshbitmap (:iconindex character))
    (makestandardsizeandorigin freshbitmap)
    ;; assign mouse handlers to expand this icon a bit when the mouse
    ;; hovers over it
    (doto freshbitmap
      (aset "mouseEnabled" true)
      (.addEventListener "mouseover" (fn [_] (highlightcharacter character)))
      (.addEventListener "mouseout" (fn [_] (unhighlightcharacter character)))
      (.addEventListener "click" (fn [_] (selectcharacter character))))
    (assoc spritemap (:uniqueid character) freshbitmap))) 

(defn- removecharactersprite [sprites character]
  ;; just dissoc and let the GC collect it, I guess
  (dissoc sprites (:uniqueid character)))

(defn- generatemissingsprites [sprites missingcharacters]
  "Looks for characters that do not have a sprite associated with them, creates any missing sprites,
and returns an updated sprite map which should have all the needed sprites."
  (reduce addcharactersprite sprites (seq missingcharacters)))

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

(defn- syncsprites [renderstate spritemapkeyword characterseq]
  "Updates the sprites used for the given spritemap. Returns the possibly-updated renderstate."
  (let [;; if spritemap is nil, defaults to {}
        sprite-path [:sprites spritemapkeyword]
        spritemap (or (get-in sprite-path renderstate) {})
        syncedspritemap (syncspritemaptocharacters spritemap characterseq)]
    ;; swap if something changed
    ;;(js/console.log (clj->js @displayed-renderstate))
    (if ((comp not identical?) spritemap syncedspritemap)
      (assoc-in renderstate sprite-path syncedspritemap)
      renderstate)))

(defn- findspriteforcharacter [renderstate spritemapkeyword character]
  (let [sprite-path [:sprites spritemapkeyword]
        spritemap (or (get-in renderstate sprite-path) {})
        characterkey (:uniqueid character)]
    (get spritemap characterkey)))

;;
;;
;; map sprite function(s)
;;
;;

(defn rebuildmapdisplaylist [renderstate oldrenderstate]
  (let [gamemap (get-in renderstate [:gamestate :map])
        oldgamemap (get-in oldrenderstate [:gamestate :map])
        gameboardcontainer (:stage-map renderstate)
        spritesheet (getspritesheet! "icons")]
    (if ((comp not identical?) gamemap oldgamemap) ; map changed
      (do
        (.removeAllChildren gameboardcontainer)
        (doseq [tile gamemap
                :let [{:keys [x y terrain]} tile
                      freshtile (createjs/BitmapAnimation. spritesheet)
                      tilex (* x tilespacing)
                      tiley (* y tilespacing)]]
          (positionsprite freshtile tilex tiley)
          (makestandardsizeandorigin freshtile)
          (.gotoAndStop freshtile terrain)
          (.addChild gameboardcontainer freshtile)))))
  renderstate)

;;
;;
;; draw characters displayed on map
;;
;;

(defn highlightcharacteronmap [renderstate character]
  (let [charactersprite (findspriteforcharacter renderstate :mapsprites character)]
    (when charactersprite (scalesprite charactersprite 1.2)))
  )

(defn unhighlightcharacteronmap [renderstate character]
  (let [charactersprite (findspriteforcharacter renderstate :mapsprites character)]
    (when charactersprite (scalesprite charactersprite 1.0)))
  )

(defn rebuildcharacterdisplaylist [renderstate]
  (let [gamestate (:gamestate renderstate)
        characters (:characters gamestate)
        newrenderstate (syncsprites renderstate :mapsprites (vals characters))
        stage-characters (:stage-characters renderstate)]
    (.removeAllChildren stage-characters)
    (doseq [character (vals characters)]
      (displaycharactermapsprite newrenderstate character))
    newrenderstate))

;;
;;
;; draw characters displayed on left/right roster lists
;;
;;
(defn- createcharacterinfopanel [renderstate character]
  (let [sprite-path [:teamGUIs :sprites]
        panel (createjs/Container.)
        oldspritemap (get-in renderstate sprite-path)
        newspritemap (addcharactersprite oldspritemap character)
        charactersprite (get newspritemap (:uniqueid character))
        ]
    (doto charactersprite
      (aset "x" 0)
      (aset "y" 0)
      )
    (doto panel
      (.addChild charactersprite))
    (assoc-in renderstate sprite-path newspritemap))
  )

(defn- removecharacterinfopanel [renderstate character]
  )

(defn- highlightcharacterinroster [renderstate character]
  (let [charactersprite (findspriteforcharacter renderstate (:team character) character)]
    (when charactersprite (scalesprite charactersprite (* 1.2 rostertilescale)))))

(defn- unhighlightcharacterinroster [renderstate character]
  (let [charactersprite (findspriteforcharacter renderstate (:team character) character)]
    (when charactersprite (scalesprite charactersprite rostertilescale))))

(defn- displaycharacterrostersprite [rostercontainer rosterspritemap character x y]
  (let [charactersprite (get rosterspritemap (:uniqueid character))]
    (positionsprite charactersprite x y)
    (makestandardsizeandorigin charactersprite)
    (scalesprite charactersprite rostertilescale)
    (.addChild rostercontainer charactersprite)))

(defn- displaycharacterinfopanel [renderstate character])

(defn- extractteamcharacters [gamestate team]
  (let [allcharacters (vals (:characters gamestate))
        teamcharacters (filter #(= (:team %1) team) allcharacters)
        orderedteamcharacters (into-array (sort-by :uniqueid teamcharacters))]
    orderedteamcharacters
    ))

(defn rebuildteamdisplaylist [renderstate team]
  (let [gamestate (:gamestate renderstate)
        roster (get-in renderstate [:teamGUIs team])
        rostercontainer (:container roster)
        teamcharacters (extractteamcharacters gamestate team)
        teamsize (count teamcharacters)]
    (.removeAllChildren rostercontainer)
    ;;(js/console.log team)
    ;;(js/console.log teamcharacters)
    ;;(syncsprites! team teamcharacters)
    (let [newrenderstate (syncsprites renderstate team teamcharacters)
          teamspritemap (get-in newrenderstate [:sprites team])]
      (doseq [characterindex (range teamsize)
              :let [character (get teamcharacters characterindex)
                    x 0
                    y (* 2 tilespacing characterindex)]]
        (displaycharacterrostersprite rostercontainer teamspritemap character x y))
      newrenderstate)))

;;
;;
;; overlay (shows selected character and possible move destinations)
;;
;;

(defn- tilexy->pixelsxy [[tilex tiley]]
  [(* tilex tilespacing) (* tiley tilespacing)])

(defn- renderoverlaycharms [locationseq renderfunc]
  (doseq [loc locationseq
          :let [[x y] loc
                [px py] (tilexy->pixelsxy [x y])]]
    (renderfunc px py)))

(defn- renderselectedcharacterbits [overlay selectedcharacter]
  (let [x (:x selectedcharacter)
        y (:y selectedcharacter)
        [px py] (tilexy->pixelsxy [x y])]
    (doto (.-graphics overlay)
          (.beginStroke "#FFF")
          (.beginFill (.getRGB createjs/Graphics 0 0 0 0))
          (.drawCircle px py 18))))

(defn- rendermovelocations [overlay movelocations]
  (let [graphics (.-graphics overlay)]
    (renderoverlaycharms
     movelocations
     (fn [x y]
       (doto graphics
         (.beginStroke "#F00" 0.5)
         (.beginStroke "#F00" 0.5)
         (.drawCircle x y 7))))))

(defn- renderattacklocations [overlay attacklocations]
  (let [graphics (.-graphics overlay)]
    (renderoverlaycharms
     attacklocations
     (fn [x y]
       (doto graphics
         (.beginStroke "#0FF" 0.5)
         (.beginFill "#0FF" 0.5)
         (.drawCircle x y 4))))))

(defn- overlayclicked [event]
  (js/console.log "overlay clicked"))

(defn rebuildoverlay [renderstate clickables]
  "Render the overlay which shows the current selected character and possible move/attack
targets.  Returns the modified renderstate."
  (let [gamestate (:gamestate renderstate)
        movelocations (:moves clickables)
        attacklocations (:attacks clickables)
        overlay (:overlay renderstate)
        selectedcharacterid (:selectedcharacterid renderstate)
        selectedcharacter (get-in gamestate [:characters selectedcharacterid])]
    ;;(js/console.log (clj->js selectedcharacter))
    (-> overlay .-graphics .clear)
    (when (not-nil? selectedcharacter)
      (renderselectedcharacterbits overlay selectedcharacter))
    (when (not-nil? movelocations)
      (rendermovelocations overlay movelocations))
    (when (not-nil? attacklocations)
      (renderattacklocations overlay attacklocations)))
  renderstate)

;;
;;
;; update to reflect new gamestate
;;


;;

(defn updategamestate! [gamestate]
  (let [oldrenderstate @displayed-renderstate
        renderstate (assoc oldrenderstate :gamestate gamestate)
        newrenderstate (-> renderstate
            (rebuildmapdisplaylist oldrenderstate)
            (rebuildcharacterdisplaylist)
            (rebuildteamdisplaylist :team1)
            (rebuildteamdisplaylist :team2)
            (rebuildoverlay nil))]
    (if
        (compare-and-set! displayed-renderstate oldrenderstate newrenderstate)
      nil ; updated renderstate
      (do ; shouldn't happen
        (js/console.log "Renderstate changed while updating! Forcing new renderstate")
        (reset! displayed-renderstate newrenderstate)))
    (redraw newrenderstate)))

;;
;;
;; initialization functions
;;
;;

(defn- createeaseljscontainer [name [x y]]
  (let [freshcontainer (createjs/Container.)]
    (doto freshcontainer
      (aset "id" name)
      (aset "name" name)
      (aset "x" x)
      (aset "y" y)
      (aset "mouseEnabled" true))
    ;;(aset "onClick" (fn [_] (js/console.log "clicked")))
    freshcontainer))

(defn- createcharacterroster [name [x y]]
  (let [rostercontainer (createeaseljscontainer name [x y])
        background (createjs/Bitmap. (getimageasset "GUIbackground"))
        roster {:panels {} :container rostercontainer}]
    (.addChild rostercontainer background)
    roster
    ))

(defn- createoverlaycontainer []
  (let [characteroutline (createjs/Shape.)
        movelocations (createjs/Shape.)
        attacklocations (createjs/Shape.)]
    ))

(defn initializeplayarea []
  (let [stage (:stage @displayed-renderstate)
        tilemap (createeaseljscontainer "tilemap" mapoffsetpixels)
        characters (createeaseljscontainer "characters" mapoffsetpixels)
        leftRoster (createcharacterroster "team1Roster" [50 100])
        rightRoster (createcharacterroster "team2Roster" [500 100])
        overlayshape (createjs/Shape.)]
    (doto stage
      .removeAllChildren
      (.addChild tilemap)
      (.addChild characters)
      (.addChild (:container leftRoster))
      (.addChild (:container rightRoster))
      (.addChild overlayshape))
    (doto overlayshape
      (aset "x" (get mapoffsetpixels 0))
      (aset "y" (get mapoffsetpixels 1))
      (.addEventListener "click" overlayclicked))
    ;; embed direct references to the map and character containers for
    ;; easy manipulation later
    (swap! displayed-renderstate assoc :stage-map tilemap :stage-characters characters)
    (swap! displayed-renderstate assoc :teamGUIs {:team1 leftRoster :team2 rightRoster :sprites {}})
    (swap! displayed-renderstate assoc :overlay overlayshape)
    ))

(defn initializerenderer [canvasname]
  (let [canvas (dom/get-element canvasname)
        stage (createjs/Stage. canvas)]
    ;; enable mouse over for tooltips, hover, etc.
    (.enableMouseOver stage 10)
    (swap! displayed-renderstate assoc :stage stage)))
