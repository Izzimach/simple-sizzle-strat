(ns simplestrat.preloader)

;; this is really just a wrapper around the PreloadJS library.
;; http://createjs.com/

(defn- placeasset [assetlist loaderinfo]
  ( let [assettype (-> loaderinfo .-item .-type)
         assetid   (-> loaderinfo .-item .-id)
         asset     (.-result loaderinfo)]
    ;; assets are grouped by type, then by id
    (assoc-in assetlist [assettype assetid] asset)))

(defn- processassets [assets]
  "Given a set of assets, sorts out the assets by type. Produces a map
  with a key for each asset type (image, sound, etc.). Values are maps
  from id to resource."
  ;; each asset has an id, type, and result (data)
  ;;(js/console.log (clj->js assets))
  (reduce placeasset {} assets))

(defn- preloadcomplete [assets completioncallback]
  "Called when all assets have been loaded by PreloadJS. Groups up the
  assets by type (image, sound, etc.) and then call the gamestart
  function with the asset list"
  (let [processed (processassets assets)
        images (get processed "image")]
    ;;    (js/console.log (clj->js processed))
    ;;    (js/console.log (clj->js (keys processed)))
    ;;    (js/console.log (clj->js images))
    (completioncallback processed)))

;;
;; preloader - loading the assets
;;

(defn preloadgame [manifest callwhendone]
  "Preloader to load all assets. Pass in the EaselJS stage (for loading
  animation) a vector of assets to load, and a function to call once
  all assets have been loaded and processed. Assets are in a map where
  each key is an asset type (image, sound, etc.) and the value is a
  second map that maps from assets ID to the actual asset."
  (let [jsmanifest (clj->js manifest)
        loader (createjs/LoadQueue.)
        ;; loadedassets really doesn't need to be an atom since JS is
        ;; single-threaded, but oh well.
        loadedassets (atom [])
        accumulateassets #(swap! loadedassets conj %1)]
    (.addEventListener loader "complete" #(preloadcomplete @loadedassets callwhendone))
    (.addEventListener loader "fileload" accumulateassets)
    (.loadManifest loader jsmanifest)))

