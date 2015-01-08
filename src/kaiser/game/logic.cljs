; Bid data structure:
; {:trump :no-trump :points 7 :bidder 2} => A bid of 7 no trump by your partner
; {:pass true :bidder 3} => A pass by the person to your right
; Past bids data structure (during the bidding phase of a round):
; [bid1 bid2 bid3]
; Card data structure:
; {:suit :spades :value 11} => Jack of spades
; Hand data structure:
; #{card1 card2 card3}
; Turn data structure:
; {:cards [card1 card2] :leader 1} or nil for no cards played
; Past turns data structure:
; [(assoc turn1 :winner 3 :points -2) (assoc turn2 :winner 2 :points 1)]
; Rules data structure
; {:scoring :traditional :bid-to-win true :points-to-win 52 :no-trump-increase 10 :min-bid 5}
; Score data structure
; [30 19]

(ns kaiser.game.logic)

(defn card-greater? 
  "Checks if a card is higher in rank than another in the current turn"
  [card1 card2 trump led-suit]
  (let [higher (> (:value card1) (:value card2))]
    (cond
      (= trump (:suit card1)) (if (= trump (:suit card2))
                                higher
                                true)
      (= trump (:suit card2)) false
      (= led-suit (:suit card1)) (if (= led-suit (:suit card2))
                                   higher
                                   true)
      (= led-suit (:suit card2)) false
      :else higher)))

(defn turn-winner 
  "Determines which player won a hand"
  [turn trump]
  (mod (+ (:leader turn)
          (:top-index
            (reduce (fn [top card]
                      (if (card-greater? card
                                         (:card top)
                                         trump
                                         (:suit (first (:cards turn))))
                        {:card card :top-index (:index top) :index (inc (:index top))}
                        (assoc top :index (inc (:index top)))))
                    {:card {:suit :spades :value 0} :index 0 :top-index 0}
                    (:cards turn))))
       4))

(defn points 
  "Calculates the point value of a turn"
  [turn]
  (let [has-three (some #(= 3 (:value %))
                        (:cards turn))
        has-five (some #(= 3 (:value %))
                       (:cards turn))]
    (+ 1
       (if has-three -3 0)
       (if has-five 5 0))))

(defn- score-traditional
  "Scores with the traditional method: bidding team doesn't get their points added on if they lose and successful no-trump bids are worth the double of the points scored"
  [points bid]
  (let [bid-team (mod (:bidder bid) 2)
        no-trump (= :no-trump (:trump bid))]
    (if (>= (nth points bid-team) (:points bid))
      (if no-trump
        (assoc points bid-team (* 2 (nth points bid-team)))
        points)
      (assoc points bid-team (* (if no-trump 2 -1)
                                (:points bid))))))

(defn- score-modified
  "Scores with the modified method: bidding team gets their points added on even if they lose and successful no-trump bids are worth the double of the bid plus the points scored"
  [points bid]
  (let [bid-team (mod (:bidder bid) 2)
        no-trump (= :no-trump (:trump bid))
        new-score (+ (nth points bid-team)
                              (* (if no-trump 2 1)
                               (* (if (< (nth points bid-team) (:points bid)) -1 1)
                                (:points bid))))]
    (if (zero? bid-team)
      [new-score (second points)]
      [(first points) new-score])))

(defn score-round
  "Calculates the change in score for each team in a round"
  [turns bid rules]
  (let [points (reduce (fn [acc turn]
                         (let [team (mod (:winner turn) 2)
                               points-change (+ (:points turn) (nth acc team))]
                           (if (zero? team)
                             [points-change (second acc)]
                             [(first acc) points-change])))
                           ;(assoc acc team (+ (:points turn) (nth acc team))))) ; Again, a victim of core.typed
                       [0 0]
                       turns)]
    ((if (= :modified (:scoring rules)) score-modified score-traditional) points bid)))

(defn game-over
  "Returns the winner if the game is over, otherwise nil"
  [score bid no-trump-bid rules]
  (let [points-to-win (+ (:points-to-win rules)
                         (if no-trump-bid (:no-trump-increase rules) 0))]
    (cond
      (= (score 0) (score 1)) nil
      (every? #(< % points-to-win) score) nil
      (:bid-to-win rules) (let [last-bid-team (mod (:bidder bid) 2)]
                            (if (> (score last-bid-team) points-to-win)
                              last-bid-team
                              nil))
      (> (score 0) (score 1)) 0
      :else 1)))

(defn bid-greater?
  "Determines whether a bid is greater than another"
  [bid1 bid2]
  (if (:pass bid1)
    false
    (or (> (:points bid1) (:points bid2))
        (and (= (:points bid1) (:points bid2))
             (= :no-trump (:trump bid1))
             (not= :no-trump (:trump bid2))))))
