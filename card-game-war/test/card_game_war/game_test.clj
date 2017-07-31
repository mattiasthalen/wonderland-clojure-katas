(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round")
    (is (= [:spade 10] (play-round [:spade 10] [:club 2])))
  (testing "queens are higher rank than jacks")
    (is (= [:spade :queen] (play-round [:spade :queen] [:club :jack])))
  (testing "kings are higher rank than queens")
    (is (= [:spade :king] (play-round [:spade :king] [:club :queen])))
  (testing "aces are higher rank than kings")
    (is (= [:spade :ace] (play-round [:spade :ace] [:club :king])))
  (testing "if the ranks are equal, clubs beat spades")
    (is (= [:club :ace] (play-round [:spade :ace] [:club :ace])))
  (testing "if the ranks are equal, diamonds beat clubs")
    (is (= [:diamond :ace] (play-round [:diamond :ace] [:club :ace])))
  (testing "if the ranks are equal, hearts beat diamonds")
    (is (= [:heart :ace] (play-round [:diamond :ace] [:heart :ace]))))

(deftest test-play-game
  (testing "the player loses when they run out of cards")
    (is (= "Player 2" (play-game (first (partition 26 cards)) (last (partition 26 cards)))))
  )