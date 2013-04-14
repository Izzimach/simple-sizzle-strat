(ns simplestrat.test.gameworld
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testin)])
  (:require [cemerick.cljs.test :as t]
            [simplestrat.gameworld :as gw]
            [simplestrat.action :as action]))

;; initial game state with and without a character
(def initialgamestate (gw/makeemptygamestate))
(def testid1 1)
(def testid2 2)
(def character1 (gw/create-character
                 {:charactername "test character"
                  :id testid1 :iconindex 1 :coords [3 3] :team :team1 :starthealth 2
                  :actions [
                            (action/createmoveaction "walk" 1 1)
                            (action/createmajoraction "punch" 1 1 1)
                            ]
                  }))

(def character2 (gw/create-character
                 {:charactername "second character"
                  :id testid2 :iconindex 2 :coords [4 4] :team :team2 :starthealth 3
                  :actions [
                            (action/createmoveaction "run" 2 2)
                            (action/createmajoraction "shoot" 2 5 2)
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
    (action/seqof-movelocationsforcharacter gamestate character nil)))

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
          whereis (comp coordsofcharacter gw/get-character)
          movechar (fn [gamestate charid [dx dy]]
                     (action/invokedefaultmoveaction gamestate (gw/get-character gamestate charid) [dx dy]))]
      (is (= [3 3] (whereis gamewithcharacter1 testid1 )) "Character should be at original location before move")
      (is (= [4 3] (whereis (movechar gamewithcharacter1 testid1 [4 3]) testid1)) "Character should be at the new location after move")
      )
    )
  )

(defn possibletargets [gamestate characterid]
  (action/seqof-attacktargetsforcharacter gamestate (gw/get-character gamestate characterid) nil))


(deftest major-actions
  (let [gamewithfarcharacters (gw/move-character gamewithbothcharacters testid2 5 4)
        gamewithreallyfarcharacters (gw/move-character gamewithbothcharacters testid2 9 9)
        counttargets (comp count possibletargets)]
    
    (is (gw/majoractionavailablefor? gamewithcharacter1 testid1) "Characters get a major action every turn")
    (is (gw/majoractionavailablefor? (gw/advanceturn gamewithcharacter2) testid2) "Characters get a major action every turn")
    (is (not (gw/majoractionavailablefor? gamewithcharacter2 testid2)) "Characters don't get major actions in their off-turn")
    
    (is (= 1 (count (action/seqof-charactermajoractions character1)))) "Character 1 has one available major action"
    (is (= 1 (count (action/seqof-charactermajoractions character2)))) "Character 2 has one available major action"
    
    (is (= 1 (counttargets  gamewithbothcharacters testid1)) "Melee actions target adjacent enemies")
    (is (= 0 (counttargets  gamewithfarcharacters testid1)) "Melee actions can't hit non-adjacent enemies")
    
    (is (= 1 (counttargets  (gw/advanceturn gamewithbothcharacters) testid2)) "Ranged actions can hit adjacent enemies")
    (is (= 1 (counttargets  (gw/advanceturn gamewithfarcharacters) testid2)) "Ranged actions can hit non-adjacent enemies")
    (is (= 0 (counttargets  (gw/advanceturn gamewithreallyfarcharacters) testid2)) "Ranged actions have a maximum range")
    
    (is (= 1 (-> gamewithcharacter1
                 (gw/damage-character testid1 1 :untyped)
                 (gw/get-character testid1)
                 :health)) "After taking one point of damage character 1 should have one health point left")
    (is (= nil (-> gamewithcharacter1
                   (gw/damage-character testid1 2 :untyped)
                   (gw/get-character testid1))) 
        "After taking two points of damage character 1 is defeated and removed from the game.")
    
    (let [countcharacters (comp count vals :characters)
          defaultattack (fn [char] (first (action/seqof-charactermajoractions char)))
          attack (fn [game charid targetid]
                   (let [char (gw/get-character game charid)
                         target (gw/get-character game targetid)]
                     (action/invokemajoraction game char (defaultattack char) [target])))
          gethealth (fn [game charid] (:health (gw/get-character game charid)))]
      (is (= 2 (countcharacters gamewithbothcharacters)) "Two characters enter")
      (is (= 1 (-> gamewithbothcharacters (gw/get-character testid1) defaultattack :damage))
          "Player 1's default attack does one damage")
      (is (= 2 (-> gamewithbothcharacters (gw/get-character testid2) defaultattack :damage))
          "Player 2's default attack does two damage")
      
      (is (= 3 (gethealth gamewithbothcharacters testid2)) "Character 2 starts with 3 health before the attack")
      (is (= 2 (-> gamewithbothcharacters
                   (attack testid1 testid2)
                   (gethealth testid2))) "Character 1 attacks character 2, reducing character 2's health from 3 to 2")
      (is (= 1 (-> gamewithbothcharacters
                   (attack testid2 testid1)
                   countcharacters)) "Character 2 attacks player one, defeating it, so only one character remains"))
    ))


