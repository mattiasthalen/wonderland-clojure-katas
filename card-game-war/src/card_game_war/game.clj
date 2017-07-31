(require '[clojure.string :as str])
(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

;shuffled deck
(def shuffled-deck (seq (shuffle cards)))

;generate player1 hand
(def player1-cards
  (take-nth 2 shuffled-deck))

;generate player2 hand
(def player2-cards
  ;return cards filtered by cards not in player1's hand
  (filter (fn [x] (not (contains? (set player1-cards) x))) shuffled-deck))

;which hand wins?
(defn play-round [player1-card player2-card]
  (let [ ;get rank of player 1's card value
         p1-val-rank (.indexOf ranks (get player1-card 1))
         ;get rank of player 2's card value
         p2-val-rank (.indexOf ranks (get player2-card 1))
         ;get rank of player 1's card suit
         p1-suit-rank (.indexOf suits (get player1-card 0))
         ;get rank of player 2's card suit
         p2-suit-rank (.indexOf suits (get player2-card 0))]
    ;Return winner
    (cond
      ;if p1-val rank is higher, p1 wins
      (> p1-val-rank p2-val-rank) player1-card
      ;if p2-val rank is higher, p2 wins
      (< p1-val-rank p2-val-rank) player2-card
      ;if none is higher, check the suits
      :else (cond
              ;if p1-suit rank is higher, p1 wins
              (> p1-suit-rank p2-suit-rank) player1-card
               ;if p2-suit rank is higher, p2 wins
              (< p1-suit-rank p2-suit-rank) player2-card
              :else "same card!"))))

;play the game!
(defn play-game [player1-cards player2-cards]
  (let [ ;setup p1 hand
         p1-hand (atom player1-cards)
         
         ;setup p2 hand
         p2-hand (atom player2-cards)]
    
    ;iterate through hands while both still have cards
    (while (every? false? [(empty? @p1-hand) (empty? @p2-hand)])
      (if
        ;check if player 1 won the round
        (= (play-round (last @p1-hand) (last @p2-hand)) (last @p1-hand))
        
        ;player 1 gets cards
        (do
          ;add cards to player 1's hand
          (reset! p1-hand
                  (->
                    ;get all cards but the last one
                    (butlast @p1-hand)
                    
                    ;add the winning card
                    (conj ,,, (last @p1-hand))
                    
                    ;add the loosing card
                    (conj ,,, (last @p2-hand))))

          ;remove card from player 2's hand
          (swap! p2-hand drop-last))
        
        ;else player 2 gets cards
        (do
          ;add cards to player 2's hand
          (reset! p2-hand
                  (->
                    ;get all cards but the last one
                    (butlast @p2-hand)
                    
                    ;add the winning card
                    (conj ,,, (last @p2-hand))
                    
                    ;add the loosing card
                    (conj ,,, (last @p1-hand))))
          
          ;remove card from player 1's hand
          (swap! p1-hand drop-last))))
        
    ;announce winner
    (if
      (empty? @p2-hand)
      "Player 1"
      "Player 2")))