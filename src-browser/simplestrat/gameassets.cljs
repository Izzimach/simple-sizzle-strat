(ns simplestrat.gameassets)

;; manifest detailing all the files used along with an id string
;; for each asset for looking them up later
(def manifest
  [
   {:id "icons" :src "gfx/placeholders_tigsource.png"}
   ]
  )


;; all game assets, keyed by type. For example (get gameassets
;; "image") will get a map of all images keyed by image id.
(def assets {})

