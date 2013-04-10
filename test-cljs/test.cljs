(ns simplestrat.test.gameworld
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testin)])
  (:require [cemerick.cljs.test :as t]
            [simplestrat.gameworld :as gw]))

;; initial game state with and without a character
(def initialgamestate (gw/makeemptygamestate))
(def testid1 1)
(def testid2 2)

(def character1 (gw/create-character
                 {:charactername "test character"
                  :id testid1 :iconindex 1 :coords [3 3] :team :team1 :starthealth 2
                  :actions [
                            (gw/createmoveaction "walk" 1 1)
                            (gw/createmajoraction "punch" 1 1 2)
                            ]
                  }))

(def character2 (gw/create-character
                 {:charactername "second character"
                  :id testid2 :iconindex 2 :coords [4 4] :team :team2 :starthealth 3
                  :actions [
                            (gw/createmoveaction "run" 2 2)
                            (gw/createmajoraction "shoot" 2 5 2)
                            ]
                  }))

(def gamewithcharacter1
  (-> initialgamestate
      (gw/put-character character1)
      gw/advanceturn))

(def gamewithcharacter2
  (-> initialgamestate
      (gw/put-character character2)
      gw/advanceturn))

(def gamewithbothcharacters
  (-> initialgamestate
      (gw/put-character character1)
      (gw/put-character character2)
      (gw/advanceturn)))

;; movement spots for a single character
(defn possiblemoves [gamestate characterid]
  (let [character (gw/get-character gamestate characterid)]
    (gw/seqof-movelocationsforcharacter gamestate character nil)))

(deftest existance-and-actions
  (let [countmoves (comp count possiblemoves)
        countactions (comp count :actionsleft)]
    (is (empty? (:characters (gw/makeemptygamestate))) "The initial game state has no characters")
    (is (not (empty? (:characters gamewithcharacter1))) "Adding a character results in a non-empty character list")

    (is (= 2 (countactions gamewithcharacter1)) "A game with one character should have two actions: one move action and one standard action")
    (is (= 0 (countactions (gw/advanceturn gamewithcharacter1))) "Characters don't get actions in their off-turn")
    
    (is (= 0 (countactions gamewithcharacter2)) "Characters don't get actions in their off-turn")
    (is (= 2 (countactions (gw/advanceturn gamewithcharacter2))) "Characters get actions in their turn")

    (is (gw/moveactionavailablefor? gamewithcharacter1 testid1) "Characters get a move action every turn")
    (is (gw/moveactionavailablefor? (gw/advanceturn gamewithcharacter2) testid2) "Characters get a move action every turn")

    (is (not (gw/moveactionavailablefor? gamewithcharacter2 testid2)) "Characters don't get move actions in their off-turn"))
  )

(deftest move-actions
  (let [countmoves (comp count possiblemoves)]
    (is (= 8 (countmoves gamewithcharacter1 testid1)) "Characters with move 1 can move to 8 adjacent tiles")
    (is (= 24 (countmoves (gw/advanceturn gamewithcharacter2) testid2)) "Characters with move 2 can move to 24 adjacent tiles")

    ;; in the gamestate with both characters-each blocks the movement of
    ;; their opponent
    (is (= 7  (countmoves gamewithbothcharacters testid1)) "Enemies block movement")
    (is (= 23 (countmoves (gw/advanceturn gamewithbothcharacters) testid2)) "Enemies block movement")

    ;; test out executing actual movement actions
    (let [coordsofcharacter (fn [char] [(:x char) (:y char)])
          whereis (comp coordsofcharacter gw/get-character)]
      (is (= [3 3] (whereis gamewithcharacter1 testid1 )) "Character should be at original location before move")
      
      ))
  
  )

(defn possibletargets [gamestate characterid]
  (gw/seqof-attacktargetsforcharacter gamestate (gw/get-character gamestate characterid) nil))


(deftest major-actions
  (let [gamewithfarcharacters (gw/move-character gamewithbothcharacters testid2 5 4)
        gamewithreallyfarcharacters (gw/move-character gamewithbothcharacters testid2 9 9)
        counttargets (comp count possibletargets)]
    (is (gw/majoractionavailablefor? gamewithcharacter1 testid1) "Characters get a major action every turn")
    (is (gw/majoractionavailablefor? (gw/advanceturn gamewithcharacter2) testid2) "Characters get a major action every turn")
    (is (not (gw/majoractionavailablefor? gamewithcharacter2 testid2)) "Characters don't get major actions in their off-turn")

    (is (= 1 (count (gw/seqof-charactermajoractions character1)))) "Character 1 has one available major action"
    (is (= 1 (count (gw/seqof-charactermajoractions character2)))) "Character 2 has one available major action"

    (is (= 1 (counttargets  gamewithbothcharacters testid1)) "Melee actions target adjacent enemies")
    (is (= 0 (counttargets  gamewithfarcharacters testid1)) "Melee actions can't hit non-adjacent enemies")

    (is (= 1 (counttargets  (gw/advanceturn gamewithbothcharacters) testid2)) "Ranged actions can hit adjacent enemies")
    (is (= 1 (counttargets  (gw/advanceturn gamewithfarcharacters) testid2)) "Ranged actions can hit non-adjacent enemies")
    (is (= 0 (counttargets  (gw/advanceturn gamewithreallyfarcharacters) testid2)) "Ranged actions have a maximum range")
    ))

