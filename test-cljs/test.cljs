(ns simplestrat.test.gamestate
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testin)])
  (:require [cemerick.cljs.test :as t]
            [simplestrat.gameworld :as gameworld]))

;; initial game state with and without a character
(def initialgamestate (gamestate/makeemptygamestate))
(def testid 1)
(def acharacter (gameworld/create-character
                 {:charactername "test character"
                  :id testid :iconindex 1 :coords [3 3] :team :team1 :starthealth 2
                  :actions [(gameworld/createstandardmoveaction "walk" 1 1)]}))
(def gamewithacharacter
  (-> initialgamestate
      (gameworld/put-character acharacter)
      gameworld/advanceturn))

;; movement spots for a single character
(def acharactermovementspots
  (let [character (gameworld/get-character gamewithacharacter testid)]
    (gameworld/seqof-movelocationsforcharacter gamewithacharacter character nil)))

(deftest characters-and-gamestate
  (is (empty? (:characters (gameworld/makeemptygamestate))) "The initial game state has no characters")
  (is (not (empty? (:characters gamewithacharacter))) "Adding a character results in a non-empty character list")
  (is (= 2 (count (:actionsleft gamewithacharacter))) "A game with one character should have two actions: one move action and one standard action")
  (is (gameworld/moveactionavailablefor gamewithacharacter (:uniqueid acharacter)) "Characters get a move action every turn")
  (is (gameworld/standardactionavailablefor gamewithacharacter (:uniqueid acharacter)) "Characters get a standard action every turn")
  (is (= 8 (count acharactermovementspots)) "Characters with move 1 can move to 8 adjacent tiles")
  )


