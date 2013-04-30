(ns simplestrat.renderstate
  (:use [clojure.set :only (difference)])
  (:require [simplestrat.gameassets :as gameassets]
            [simplestrat.gameworld :as world]
            [simplestrat.action :as action]
            [clojure.browser.dom :as dom]
            [clojure.string :as string]))


(def ^:const ^:private spritescale 2)
(def ^:const ^:private basetilesize 12)
(def ^:const ^:private standardtilesize (* spritescale basetilesize))
(def ^:const ^:private tilespacing standardtilesize)
(def ^:const ^:private rostertilescale 1.8) ; relative to standardtilesize
(def ^:const ^:private rostertilesize (* rostertilescale standardtilesize))

(def ^:private not-nil? (comp not nil?))


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

;; submitplayeraction is set by an external caller (usually core). When you want to modifiy
;; the global game state, create a function to modify the game state and "send" it to
;; submitplayeraction
(def *submitplayeraction*)
(def *restartgame*)

(defn setactionhook! [submitfunc]
  (set! *submitplayeraction* submitfunc))

(defn setrestarthook! [restartfunc]
  (set! *restartgame* restartfunc))
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
    ;; no spritesheet with this name yet, create it and add to the cache
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

(defn setcanvascaption [renderstate caption]
  (let [stage (:stage renderstate)
        canvas (.-canvas stage)]
    (aset canvas "title" caption)))

(defn- highlightcharacter
  "Highlights current character. Doesn't modify the clojure-side renderstate"
  [character]
  (let [renderstate @displayed-renderstate]
    (doto renderstate
      (highlightcharacteronmap character)
      (highlightcharacterinroster character)
      (setcanvascaption (:name character))
      (redraw))))

(defn- unhighlightcharacter [character]
  (let [renderstate @displayed-renderstate]
    (doto renderstate
      (unhighlightcharacteronmap character)
      (unhighlightcharacterinroster character)
      (setcanvascaption "")
      (redraw))))

(defn getclickablesfor
  [gamestate character selectedaction]
  (let [majoraction (if (action/ismajoraction? selectedaction)
                      selectedaction 
                      (action/getdefaultmajoraction character))
        moveaction (if (action/ismoveaction? selectedaction)
                     selectedaction
                     (action/getdefaultmoveaction character))
        characterid (:uniqueid character)
        attacktargets (if (world/majoractionavailablefor? gamestate characterid)
                        (action/seqof-attacktargetsforcharacter gamestate character majoraction)
                        [])
        movedestinations (if (world/moveactionavailablefor? gamestate characterid)
                           (action/seqof-movelocationsforcharacter gamestate character moveaction)
                           [])]
    {:moves movedestinations :attacks attacktargets})
  )

(defn- selectcharacter [character]
  (let [uniqueid (:uniqueid character)
        defaultmajoraction (action/getdefaultmajoraction character)
        defaultmoveaction (action/getdefaultmoveaction character)
        renderstate (assoc @displayed-renderstate :selectedcharacterid uniqueid :selectedaction defaultmajoraction)
        gamestate (:gamestate renderstate)
        overlaydata (getclickablesfor gamestate character defaultmajoraction)
        newrenderstate (rebuildoverlay renderstate overlaydata)]
    (reset! displayed-renderstate newrenderstate)
    #_(swap! displayed-renderstate #(rebuildoverlay % overlaydata))
    (redraw newrenderstate)))

(defn- selectcharacteraction [character active-action]
  (let [uniqueid (:uniqueid character)
        renderstate (assoc @displayed-renderstate :selectedcharacterid uniqueid :selectedaction active-action)
        gamestate (:gamestate renderstate)
        overlaydata (getclickablesfor gamestate character active-action)
        newrenderstate (rebuildoverlay renderstate overlaydata)
        ]
    (reset! displayed-renderstate newrenderstate)
    (redraw newrenderstate))
  )

;;
;;
;; character sprite functions
;;
;;
;; current sprite lists [:mapsprites :team1 :team2]
;;

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

(defn- addactionsprite [spritemap actiondata]
  (let [freshbitmap (createjs/BitmapAnimation. (getspritesheet! "icons"))
        standardscale (* 0.5 spritescale)
        hightlightedscale (* 0.6 spritescale)]
    (.gotoAndStop freshbitmap (:iconindex actiondata))
    (makestandardsizeandorigin freshbitmap)
    (scalesprite freshbitmap 0.75)
    (doto freshbitmap
      (aset "mouseEnabled" true)
      (.addEventListener "mouseover" (fn [_] (setcanvascaption @displayed-renderstate (:description actiondata))))
      (.addEventListener "mouseout" (fn [_] (setcanvascaption @displayed-renderstate "")))
      (.addEventListener "click" (fn [_] nil)))
    (assoc spritemap (:uniqueid actiondata) freshbitmap)))

(defn- addactionsprites [spritemap character]
  (reduce addactionsprite spritemap (:actions character)))

(defn- removecharactersprite [sprites character]
  ;; just dissoc and let the GC collect it, I guess
  (dissoc sprites (:uniqueid character)))

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
        ((partial reduce addcharactersprite) (seq characterseq))
        ((partial reduce removecharactersprite) (seq unusedspriteids)))))

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
    (if ((comp not identical?) gamemap oldgamemap) ;; map changed
      (do
        (.removeAllChildren gameboardcontainer)
        (doseq [tile (:tiledata gamemap)
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
;; and info tiles
;;

(defn- createcharacterroster [name team [x y]]
  (let [rostercontainer (createeaseljscontainer name [x y])
        ;;background (createjs/Bitmap. (getimageasset "GUIbackground"))
        roster {:panels {} :team team :container rostercontainer}]
    ;;(.addChild rostercontainer background)
    roster
    ))

(defn- createcharacterinfopanel [renderstate character orderindex]
  (let [rostercontainer (get-in renderstate [:teamGUIs (:team character) :container])
        sprite-path [:sprites (:team character)]
        panel (createjs/Container.)
        charactername (createjs/Text.)
        healthtext (createjs/Text. "Health: 0")
        actionsavailabletext (createjs/Text. "Actions Available: ")
        oldspritemap (get-in renderstate sprite-path)
        newspritemap (-> oldspritemap
                       (addcharactersprite character)
                       (addactionsprites character))
        charactersprite (get newspritemap (:uniqueid character))]
    (doto charactername
      (aset "text" (:name character))
      (aset "x" 30)
      (aset "y" -24))
    (doto charactersprite
      (aset "x" 0)
      (aset "y" 0)
      (aset "name" "sprite")
      (scalesprite rostertilescale))
    (doto healthtext
      (aset "x" 30)
      (aset "y" -15)
      (aset "name" "healthtext"))
    (doto actionsavailabletext
      (aset "x" 30)
      (aset "y" -7)
      (aset "name" "actiontext"))
    (doto panel
      (aset "name" (:uniqueid character))
      (.addChild charactername)
      (.addChild charactersprite)
      (.addChild healthtext)
      (.addChild actionsavailabletext)
      (aset "y" (* orderindex 45)))
    (doseq [[actionindex actiondata] (map-indexed (fn [a b] [a b]) (:actions character))
            :let [actionsprite (get newspritemap (:uniqueid actiondata))]]
      (doto actionsprite
        (aset "x" (+ 40 (* 20 actionindex)))
        (aset "y" 13))
      (.addChild panel actionsprite))
    (.addChild rostercontainer panel)
    (-> renderstate
        (assoc-in sprite-path newspritemap)
        (assoc-in [:teamGUIs (:team character) :panels (:uniqueid character)] panel))))

(defn- removecharacterinfopanel [renderstate character]
  (let [panelpath [:teamGUIs (:team character) :panels (:uniqueid character)]
        panel (get-in renderstate panelpath)
        rostercontainer (get-in renderstate [:teamGUIs (:team character) :container])]
    (.removeChild rostercontainer panel)
    (utils/dissoc-in renderstate panelpath)
  ))

(defn- getactionwords [renderstate character]
  (let [gamestate (:gamestate renderstate)
        characterid (:uniqueid character)
        ;; if the function returns true, conj's actiontext onto actionwords
        checkforaction (fn [actionwords actioncheckfunc actiontext] (if (actioncheckfunc gamestate characterid) (conj actionwords actiontext) actionwords))]
    (-> ["Actions: "]
        (checkforaction world/majoractionavailablefor? "Attack")
        (checkforaction world/moveactionavailablefor? "Move")
        )))

(defn- updatepaneldata [renderstate character orderindex]
  (let [panelpath [:teamGUIs (:team character) :panels (:uniqueid character)]
        panel (get-in renderstate panelpath)
        healthtext (.getChildByName panel "healthtext")
        actiontext (.getChildByName panel "actiontext")
        actionwords (getactionwords renderstate character)]
    (aset healthtext "text" (string/join " " ["Health:" (:health character)]))
    (aset actiontext "text" (string/join " " actionwords))
    renderstate
    )
  )

(defn- updatecharacterinfopanel [renderstate [orderindex character]]
  (let [roster (get-in renderstate [:teamGUIs (:team character)])
        {:keys [container panels]} roster
        characterid (:uniqueid character)]
    ;; update the panels if it exists; if it doesn't create a new panel
    (if (contains? panels characterid)
      (updatepaneldata renderstate character orderindex)
      (-> renderstate
          (createcharacterinfopanel character orderindex)
          (updatepaneldata character orderindex))
      )
    )
  )

(defn- removedeadcharacterpanels [renderstate characters team]
  ;; walk over all children in the container and remove any that don't
  ;; have a corresponding character
  (let [roster (get-in renderstate [:teamGUIs team])
        {:keys [container panels]} roster
        numchildren (.getNumChildren container)
        panelinstances (doall (map #(.getChildAt container %) (range numchildren)))]
    ;;(js/console.log (clj->js characters))
    (doseq [childpanel panelinstances
            :let [childid (.-name childpanel)]]
      ;;(js/console.log childcontainer)
      ;;(js/console.log childid)
      (when (not-any? #(= childid (:uniqueid %)) characters)
        ;;(js/console.log "removing")
        (.removeChild container childpanel)))
    ;; TODO remove info panel from the "panels" field of the roster as well!
    renderstate))

(defn syncpanelstoteamcharacters
  [renderstate rostercontainer characterpanels teamcharacters]
  (let [panelset (set (keys characterpanels))]
    (reduce updatecharacterinfopanel renderstate (map-indexed (fn [ix ch] [ix ch]) teamcharacters))
    ))

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
        characterpanels (:panels roster)
        teamcharacters (extractteamcharacters gamestate team)
        teamsize (count teamcharacters)]
    ;;(.removeAllChildren rostercontainer)
    ;;(js/console.log team)
    ;;(js/console.log teamcharacters)
    ;;(syncsprites! team teamcharacters)
    (-> renderstate
        (syncpanelstoteamcharacters rostercontainer characterpanels teamcharacters)
        (removedeadcharacterpanels teamcharacters team)        )    ))

;;
;;
;; overlay (shows selected character and possible move destinations)
;;
;;

(defn- tilexy->pixelsxy [[tilex tiley]]
  [(* tilex tilespacing) (* tiley tilespacing)])

(defn- pixelsxy->tilexy [[pixelx pixely]]
  (let [scaledown #(js/Math.round (/ % tilespacing))]
    [(scaledown pixelx) (scaledown pixely)]))

(defn- renderoverlaycharms [locationseq renderfunc]
  (doseq [actionloc-pair locationseq
          :let [[action loc] actionloc-pair
                [px py] (tilexy->pixelsxy loc)]]
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
         (.beginStroke "#0EE" 0.5)
         (.beginFill "#0FF" 0.5)
         (.drawCircle x y 7))))
    ))

(defn- renderattacktargets [overlay attacktargets]
  (let [graphics (.-graphics overlay)
        renderfunc (fn [x y]
                     (doto graphics
                       (.beginStroke "#F00" 0.6)
                       (.beginFill "#A00" 0.6)
                       (.drawCircle x y 6)))
        ]
    (doseq [actiontarget-pair attacktargets
            :let [[action {:keys [x y]}] actiontarget-pair
                  [px py] (tilexy->pixelsxy [x y])]
            ]
      (renderfunc px py))))

(defn- overlaymouseover [event]
  (let [renderstate @displayed-renderstate
        overlay (:overlay renderstate)
        gamestate (:gamestate renderstate)
        mouseoverpx (- (aget event "stageX") (.-x overlay))
        mouseoverpy (- (aget event "stageY") (.-y overlay))
        [tilex tiley] (pixelsxy->tilexy [mouseoverpx mouseoverpy])
        ;; keys in clickables are of the form [x y]
        mouseoverdata (:overlaymouseoverdata renderstate) 
        mouseondata (mouseoverdata [tilex tiley])
        ]
    (when mouseondata
      (setcanvascaption renderstate mouseondata))))

(defn- overlaymouseout [event]
  (let [renderstate @displayed-renderstate]
    (setcanvascaption renderstate "")))

(defn- overlayclicked [event]
  (let [renderstate @displayed-renderstate
        overlay (:overlay renderstate)
        gamestate (:gamestate renderstate)
        clickpx (- (aget event "stageX") (.-x overlay))
        clickpy (- (aget event "stageY") (.-y overlay))
        [tilex tiley] (pixelsxy->tilexy [clickpx clickpy])
        ;; keys in clickables are of the form [x y]
        clickcallbacks (:overlayclickables renderstate) 
        clickedon (clickcallbacks [tilex tiley])
        ]
    ;;(js/console.log "overlay clicked")
    ;;(js/console.log event)
    ;; lookup the clicked tile to see if there is a clickable there
    ;;(js/console.log (clj->js [tilex tiley]))
    ;;(js/console.log clickedon)
    (if clickedon (clickedon))
    ))

(defn- movecallback
  [gamestate character action [targetx targety]]
  ;;(js/console.log (clj->js [character action]))
  (*submitplayeraction* #(action/invokemoveaction gamestate character action [targetx targety])))

(defn- attackcallback
  [gamestate character action target]
  (*submitplayeraction* #(action/invokemajoraction gamestate character action [target])))

(defn- buildclickcallbacks
  [renderstate actiondata]
  (let [{:keys [moves attacks]} actiondata
        {:keys [gamestate selectedcharacterid selectedaction]} renderstate
        character (world/get-character gamestate selectedcharacterid)
        addmovecallback (fn [callbacks [action location]] 
                          (assoc callbacks location #(movecallback gamestate character action location)))
        addattackcallback (fn [callbacks [action target]]
                            (let [{:keys [x y]} target
                                  location [x y]]
                              (assoc callbacks location #(attackcallback gamestate character action target))))
        ]
    ;; each callback is a function with parameters [gamestate character action targetx targety]
    ;; convert move data into clickcallbacks
    (merge
      (reduce addmovecallback {} moves)
      (reduce addattackcallback {} attacks))))

(defn- buildmouseoverdata
  [renderstate actiondata]
  (let [{:keys [moves attacks]} actiondata
        {:keys [gamestate selectedcharacterid selectedaction]} renderstate
        character (world/get-character gamestate selectedcharacterid)
        addmovedata (fn [mouseoverdata [action location]] 
                          (assoc mouseoverdata location (string/join ["Move here using '" (:name action) "'"])))
        addattackdata (fn [mouseoverdata [action target]]
                            (let [{:keys [x y]} target
                                  location [x y]]
                              (assoc mouseoverdata location (string/join ["Attack here using '" (:name action) "'"]))))
        ]
    ;; each callback is a function with parameters [gamestate character action targetx targety]
    ;; convert move data into clickcallbacks
    (merge
      (reduce addmovedata {} moves)
      (reduce addattackdata {} attacks))))


(defn rebuildoverlay
  "Render the overlay which shows the current selected character and possible move/attack
  targets.  Returns the modified renderstate."
  [renderstate actiondata]
  (let [gamestate (:gamestate renderstate)
        overlay (:overlay renderstate)
        selectedcharacterid (:selectedcharacterid renderstate)
        movelocations (:moves actiondata)
        attacktargets (:attacks actiondata)
        selectedcharacter (world/get-character gamestate selectedcharacterid)]
    ;;(js/console.log (clj->js selectedcharacter))
    (-> overlay .-graphics .clear)
    (when (not-nil? selectedcharacter)
      (renderselectedcharacterbits overlay selectedcharacter))
    (when (not-nil? movelocations)
      (rendermovelocations overlay movelocations))
    (when (not-nil? attacktargets)
      (renderattacktargets overlay attacktargets))
    (-> renderstate 
      (assoc :overlayclickables (buildclickcallbacks renderstate actiondata))
      (assoc :overlaymouseoverdata (buildmouseoverdata renderstate actiondata)))))

;;
;; text log window
;;

(defn createmessagelog [[x y]]
  (let [logdisplay (createjs/Text. "argh" "12px 'Autour One'" "#222")]
    (doto logdisplay
      (aset "x" x)
      (aset "y" y)
      (aset "text" "Game started.")
      )
    logdisplay
    ))
  
(defn updatemessagelog [renderstate]
  (let [{:keys [gamestate messagelog]} renderstate
        first10messages (take 10 (:loglist gamestate))]
    (aset messagelog "text" (clojure.string/join "\n" (reverse first10messages)))
    renderstate))

;;
;;
;; other UI (turn over, etc.)
;;
;;

(defn- computerturn [gamestate]
  (-> gamestate
    world/advanceturn
    simplestrat.minimaxAI/executecomputerturn
    world/advanceturn))

(defn- teamXwon? [gamestate team]
  (let [otherteam (world/nextturnteamfrom team)]
    (= 0 (count (world/charactersforteam gamestate otherteam)))))

(defn- playerwon? [gamestate] (teamXwon? gamestate :team1))

(defn- computerwon? [gamestate] (teamXwon? gamestate :team2))

(defn- makedialogGUI [text]
  (let [newpanel (createjs/Container.)
        newtext (createjs/Text. text "30px Arial")
        bkg (createjs/Shape.)
        bkg-graphics (.-graphics bkg)
        renderstate @displayed-renderstate
        stage (:stage renderstate)]
    (doto bkg-graphics
      (.beginFill "#a0a0a0")
      (.drawRect -180 0 360 35))
    (doto newtext
      (aset "x" 0)
      (aset "textAlign" "center"))
    (doto newpanel
      (aset "x" 300)
      (aset "y" 200)
      (.addChild bkg)
      (.addChild newtext)
      (.addEventListener "click" (fn [_] (js/console.log "restarting") (*restartgame*))))
    (.addChild stage newpanel)
    (redraw renderstate)))

(defn- makewinnerGUI []
  (js/console.log "Player won!")
  (makedialogGUI "You win! Click to restart"))

(defn- makeloserGUI []
  (js/console.log "Computer won!")
  (makedialogGUI "You lose! Click to restart"))

(defn endturnclicked [event]
  (let [renderstate @displayed-renderstate
        gamestate (:gamestate renderstate)
        endturnbutton (.getChildByName (:controlpanel renderstate) "text")
        processcomputerturn (fn []
                              (let [newgamestate (*submitplayeraction* computerturn)]
                                (doto endturnbutton
                                  (aset "text" "End My Turn")
                                  (aset "color" "#0aa"))
                                (redraw @displayed-renderstate)))
        ]
    (when (= :team1 (:activeteam gamestate))
      (doto endturnbutton
        (aset "color" "#A22")
        (aset "text" "THINKING...")
        )
      (redraw renderstate)
      (js/setTimeout processcomputerturn 16))))


(defn createcontrolpanel [[x y]]
  (let [controlpanel (createjs/Container.)
        controlpaneltext (createjs/Text. "End My Turn" "30px Arial" "#0aa")
        controlpanelbkg (createjs/Shape.)
        bkg-graphics (.-graphics controlpanelbkg)]
    (doto bkg-graphics
      (.beginFill "#222")
      (.drawRect -100 0 200 32))
    (doto controlpanel
      (aset "x" x)
      (aset "y" y)
      (.addChild controlpanelbkg)
      (.addChild controlpaneltext)
      (.addEventListener "click" endturnclicked))
    (doto controlpaneltext
      (aset "name" "text")
      (aset "textAlign" "center"))
    controlpanel))

;;
;;
;; update to reflect new gamestate
;;


(defn updategamestate! [gamestate]
  (let [oldrenderstate @displayed-renderstate
        renderstate (assoc oldrenderstate :gamestate gamestate)
        selectedcharacterid (:selectedcharacterid renderstate)
        gamestate (:gamestate renderstate)
        overlaydata (if selectedcharacterid
                      (getclickablesfor gamestate (world/get-character gamestate selectedcharacterid) nil)
                      nil)
        newrenderstate (-> renderstate
            (rebuildmapdisplaylist oldrenderstate)
            (rebuildcharacterdisplaylist)
            (rebuildteamdisplaylist :team1)
            (rebuildteamdisplaylist :team2)
            (rebuildoverlay overlaydata)
            (updatemessagelog))]
    (if (compare-and-set! displayed-renderstate oldrenderstate newrenderstate)
      nil ; updated renderstate
      (do ; shouldn't happen
        (js/console.log "Renderstate changed while updating! Forcing new renderstate")
        (reset! displayed-renderstate newrenderstate)))
    (cond
      (playerwon? gamestate) (makewinnerGUI)
      (computerwon? gamestate) (makeloserGUI))
    (redraw newrenderstate)))

;;
;;
;; initialization functions
;;
;;
(def ^:const ^:private mapoffsetpixels [180 14])


(defn initializeplayarea []
  (let [stage (:stage @displayed-renderstate)
        tilemap (createeaseljscontainer "tilemap" mapoffsetpixels)
        characters (createeaseljscontainer "characters" mapoffsetpixels)
        leftRoster (createcharacterroster "team1Roster" :team1 [30 60])
        rightRoster (createcharacterroster "team2Roster" :team2 [400 60])
        overlayshape (createjs/Shape.)
        controlpanel (createcontrolpanel [200 200])
        messagelog (createmessagelog [10 230])]
    (doto stage
      .removeAllChildren
      (.addChild tilemap)
      (.addChild characters)
      (.addChild (:container leftRoster))
      (.addChild (:container rightRoster))
      (.addChild overlayshape)
      (.addChild messagelog)
      (.addChild controlpanel))
    (doto overlayshape
      (aset "x" (get mapoffsetpixels 0))
      (aset "y" (get mapoffsetpixels 1))
      (aset "mouseEnabled" true)
      (.addEventListener "click" overlayclicked)
      (.addEventListener "mouseover" overlaymouseover)
      (.addEventListener "mouseout" overlaymouseout))
    ;; embed direct references to the map and character containers for
    ;; easy manipulation later
    (swap! displayed-renderstate assoc :stage-map tilemap :stage-characters characters)
    (swap! displayed-renderstate assoc :teamGUIs {:team1 leftRoster :team2 rightRoster :sprites {}})
    (swap! displayed-renderstate assoc :overlay overlayshape :messagelog messagelog :controlpanel controlpanel)
    ))

(defn initializerenderer [canvasname]
  (let [canvas (dom/get-element canvasname)
        stage (createjs/Stage. canvas)]
    ;; enable mouse over for tooltips, hover, etc.
    (.enableMouseOver stage 10)
    (swap! displayed-renderstate assoc :stage stage)))
