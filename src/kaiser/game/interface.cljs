(ns kaiser.game.interface
  (:require [kaiser.game.logic :as logic]))

(def ^:private cards
  (into #{}
        (for [value (range 7 15)
              suit #{:hearts :spades :diamonds :clubs}]
          (if (and (= value 7)
                   (#{:hearts :spades} suit))
            (if (= suit :hearts)
              {:value 5 :suit :hearts}
              {:value 3 :suit :spades})
            {:value value :suit suit}))))

(def ^:private default-rules
  {:scoring :traditional
   :bid-to-win true
   :points-to-win 52
   :no-trump-increase 10
   :min-bid 5})

(defn new-game
  "Creates a new kaiser game"
  ([] (new-game default-rules))
  ([rules] (new-game rules (Math/floor (* 4 (Math/random)))))
  ([rules dealer]
   {:score [0 0]
   :dealer dealer
   :rules (conj default-rules rules)
   :ended false
   :winner nil
   :no-trump-bid false}))

(defn new-round
  "Creates a new round"
  ([game] (new-round game (map set (partition 8 (shuffle cards)))))
  ([game hands]
   {:hands hands
   :dealer (:dealer game)
   :no-trump-bid false
   :bids []
   :bid nil
   :turns []
   :turn nil}))

(defn bid-valid?
  "Determines whether a bid is valid in the current round and game"
  [bid round game]
  (or (and (< (count (:bids round)) 3)
           (nil? bid))
      (let [last-bid (last (filter :points (:bids round)))] ; Check to see if this will work with nils in the list of bids
        (and (<= (:min-bid (:rules game)) (:points bid))
             (>= 12 (:points bid))
             (or (not last-bid)
                 (logic/bid-greater? bid last-bid))))))

(defn valid-bids
  "Returns a set of the valid bids or #{} if there are none"
  [round game]
  (set (filter #(bid-valid? % round game)
               (conj (for [points (range 13)
                                  suit #{:hearts :clubs :diamonds :spades :no-trump}]
                       {:points points :suits suit})
                     nil))))

(defn make-bid
  "Returns the round if the bid was valid or nil otherwise"
  [bid round game]
  (when (bid-valid? bid round game)
    (assoc round :bids (conj (:bids round) bid))))

(defn apply-bids
  "Sets the winning bid in round"
  [round]
  (assoc round :bid (last (filter :points (:bids round)))))

(defn current-player
  "Returns the player whose turn it is to play"
  [round]
  (cond
    (= 4 (count (:turn round))) (logic/turn-winner (:turn round) (:trump (:bid round)))
    (not (nil? (:turn round))) (mod (+ (:leader (:turn round))
                                       (count (:cards (:turn round))))
                                    4)
    (seq (:turns round)) (:winner (last (:turns round)))
    :else (:bidder (:bid round))))

(defn- update-turn
  "Updates :turn and :turns with the played card"
  [card player round]
  (cond
    (nil? (:turn round)) (assoc round :turn {:cards [card] :leader player})
    (not= 4 (count (:turn round))) (assoc-in round [:turn :cards] (conj (:cards (:turns round)) card))
    :else (assoc round
                 :turn {:cards [card] :leader player}
                 :turns (conj (:turns round) (assoc (:turn round)
                                                    :winner (logic/turn-winner (:turn round) (:trump (:bid round)))
                                                    :points (logic/points (:turn round)))))))

(defn play-card
  "Plays a card and returns the round or nil if the play was invalid"
  [card round]
  (let [player (current-player round)]
    (if (not (((:hands round) player) card))
      nil
      (update-turn card player (assoc-in round
                                         [:hands player] (disj ((:hands round) player) card))))))

(defn end-round
  "Scores the round and updates the game"
  [round game]
  (let [score (map + (:score game) (logic/score-round (:turns round) (:bid round) (:rules game)))
        no-trump-bid (or (:no-trump-bid game) (= :no-trump (:bid round)))
        winner (logic/game-over score (:bid round) no-trump-bid (:rules game))]
    (assoc game
           :score score
           :dealer (mod (inc (:dealer game)) 4)
           :ended (not (nil? winner))
           :winner winner
           :no-trump-bid no-trump-bid)))
