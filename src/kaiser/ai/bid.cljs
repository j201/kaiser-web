(ns kaiser.ai.bid "Exposes (ai-bid game round position looseness), which generates an AI bid")

; Calculate expected points in round for each trump possibility, then determine if it's worth choosing any

(defn- points-from-three [hand trump]
  (let [has-three (hand {:suit :spades :value 3})
        other-spades (set (filter #(and (= :spades (:suit %))
                                        (not= 3 (:value %)))
                                  hand))]
    (if has-three
      (if (= 0 (count other-spades))
        -1
        -0.6)
      (if (some #(< (:value %) 11) other-spades)
        (if (= trump :spades)
          -2
          -1.2)
        -2.5))))

(defn- points-from-five [hand trump]
  (let [has-five (hand {:suit :hearts :value 5})
        other-hearts (set (filter #(and (= :hearts (:suit %))
                                        (not= 5 (:value %)))
                                  hand))]
    (if has-five
      (if (= 0 (count other-hearts))
        2.5
        (if (= trump :hearts)
          4
          3.5))
      (if (= trump :hearts)
        (if (= 0 (count other-hearts))
          2
          (if (or (other-hearts {:suit :hearts :value 14})
                  (> (count other-hearts) 2))
            3
            2.5))
        (if (> (count (filter #(= trump (:suit %)) hand)) 2)
          3.5
          2.5)))))

(defn- top-in-suit [hand suit] ; Doesn't work with A-3 spades or A-5 hearts - very unlikely
  (loop [value 14]
    (if (hand {:suit suit :value value})
      (recur (dec value))
      (- 14 value))))

(defn- other-points [hand trump]
  (let [trump-cards (set (filter #(= trump (:suit %)) hand))
        top-trumps (top-in-suit hand trump) 
        non-trump-cards (set (filter #(not= trump (:suit %)) hand))
        non-trump-aces (count (filter #(= 14 (:value %)) non-trump-cards))
        top-non-trumps (- (reduce + (map #(top-in-suit hand %) #{:hearts :spades :diamonds :clubs})) top-trumps)]
    (if (= trump :no-trump)
      (+ 2 top-non-trumps)
      (+ 2 ; Partner's tricks
         top-trumps
         non-trump-aces
         (/ (- (count trump-cards) top-trumps) 2)
         (/ (- top-non-trumps non-trump-aces) 2)))))

(defn- expected-points [dealer position hand trump]
  (+ (points-from-three hand trump)
     (points-from-five hand trump)
     (other-points hand trump)))

; Debug and demo code begins
(defn- expected-points-debug [dealer position hand trump]
    [(points-from-three hand trump)
     (points-from-five hand trump)
     (other-points hand trump)])

(defn- show-bids [hand]
  (println (map #(-> % Math/floor dec int) ; looseness of -1
                (map #(expected-points nil
                                       nil
                                       hand
                                       %)
                     [:hearts :clubs :diamonds :spades :no-trump]))))

(defn- show-bids-debug [hand]
  (println (map #(map float %)
                (map #(expected-points-debug nil
                                             nil
                                             hand
                                             %)
                     [:hearts :clubs :diamonds :spades :no-trump]))))

#_(show-bids-debug #{{:value 9, :suit :clubs} {:value 10, :suit :diamonds} {:value 12, :suit :spades} {:value 14, :suit :hearts} {:value 14, :suit :spades} {:value 8, :suit :hearts} {:value 7, :suit :clubs} {:value 8, :suit :spades}}
             )
 
; Debug and demo code ends

(defn- adjusted-scores [dealer position hand looseness]
  (map #(hash-map :suit %
                  :points (+ looseness 
                            (expected-points dealer position hand %)))
       [:hearts :clubs :diamonds :spades :no-trump]))

; NOT TESTED
(defn ai-bid  ; Looseness is a number added to the expected points, and a bid will be made if the adjusted expected points is greated than the last bid
  ([game round position looseness]
   (ai-bid (:dealer game)
           (:min-bid (:rules game))
           ((:hands round) position)
           position
           (last (filter :points (:bids round)))
           looseness))
  ([dealer min-bid hand position last-bid looseness]
   (let [min-new-bid (or last-bid {:points (dec min-bid)})
         expected-scores (adjusted-scores dealer position hand looseness)
         best-score (Math/floor (last (sort-by :points expected-scores)))
         no-trump-score (last expected-scores)]
     (if (and (>= (:points no-trump-score) (:points min-new-bid))
              (> (* 2 (:points no-trump-score)) (:points best-score)))
       no-trump-score
       (when ((if (= dealer position) >= >)
              (:points best-score)
              (:points min-new-bid))
         best-score)))))
