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
    
    ;Return winner
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
      ;announce player1 as winner it player2's is empty
      (empty? player2-hand) 0
      ;announce player2 as winner it player1's is empty
      (empty? player1-hand) 1
      
      ;else keep goint 
      :else (let [ ;winner
                   winner (play-round player1-hand player2-hand)
                   
                   ;define winning hands
                   player1-winning-hand (->
                                          ;grab all cards, skipping the last one, since it will be moved
                                          (butlast player1-hand)
                                          
                                          ;add the cards that was just played
                                          (conj ,,, (last player1-hand))
                                          (conj ,,, (last player2-hand)))

                   player2-winning-hand (->
                                          ;grab all cards, skipping the last one, since it will be moved
                                          (butlast player2-hand)
                                          
                                          ;add the cards that was just played
                                          (conj ,,, (last player2-hand))
                                          (conj ,,, (last player1-hand)))]
              
              ;check who won the round
              (if (= winner 0)
                ;loop with player1 as winner
                (recur player1-winning-hand (butlast player2-hand))
                
                ;loop with player2 as winner
                (recur (butlast player1-hand) player2-winning-hand))))))