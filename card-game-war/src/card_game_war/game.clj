(require '[clojure.string :as str])
(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

;which hand wins?
(defn play-round [player1-card player2-card]
  (let [ ;get scores for card
         rank-score (fn [card] (.indexOf ranks (last card)))
         suit-score (fn [card] (.indexOf suits (first card)))
         
         ;get score of player 1's card
         p1-rank-score (rank-score player1-card)
         p1-suit-score (suit-score player1-card)
         
         ;get score of player 2's card
         p2-rank-score (rank-score player2-card)
         p2-suit-score (suit-score player2-card)]
    
    ;check if the cards have the same rank
    (if (= p1-rank-score p2-rank-score)
      ;if they do, who has the highest suit
      (if (> p1-suit-score p2-suit-score) 0 1)
      ;if not, who has the highest rank
      (if (> p1-rank-score p2-rank-score) 0 1))))

;play the game!
(defn play-game [player1-cards player2-cards]
  ;loop until someone runs out of cards
  (loop [ ;setup hands - reverse to make use of conj later on
         player1-hand (reverse player1-cards)
         player2-hand (reverse player2-cards)]
    
    ;stop if someone is out of cards
    (cond
      (empty? player2-hand) 0
      (empty? player1-hand) 1
      
      ;else keep playing 
      :else (let [ ;winner
                   winner (play-round (last player1-hand) (last player2-hand))
                   
                   ;winning hand
                   winning-hand (fn [winner looser]
                                  (->
                                    (butlast winner)
                                    (conj ,,, (last winner))
                                    (conj ,,, (last looser))))
                   
                   ;loosing hand
                   loosing-hand (fn [looser] (butlast looser))
                   
                   ;define loosing hands
                   player1-loosing-hand (loosing-hand player1-hand)
                   player2-loosing-hand (loosing-hand player2-hand)
                   
                   ;define winning hands
                   player1-winning-hand (winning-hand player1-hand player2-hand)
                   player2-winning-hand (winning-hand player2-hand player1-hand)]
              
              ;check who won the round and recur accordingly
              (if (= winner 0)
                (recur player1-winning-hand player2-loosing-hand)
                (recur player1-loosing-hand player2-winning-hand))))))