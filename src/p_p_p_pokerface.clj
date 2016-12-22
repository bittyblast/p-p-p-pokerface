(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

;;(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;;(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;;(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;;(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;;(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;;(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;;(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;;(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;;(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;;(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;;(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;;(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn high-card? [hand]
  true)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
    (Integer/valueOf (str fst)) (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn max-same-ranks [hand]
  (let [ranks-in-hand (map rank hand)]
    (apply max(vals(frequencies ranks-in-hand)))))

(defn pair? [hand]
  (if (>= (max-same-ranks hand) 2) true false))

(defn three-of-a-kind? [hand]
  (if (>= (max-same-ranks hand) 3) true false))

(defn four-of-a-kind? [hand]
  (if (>= (max-same-ranks hand) 4) true false))

(defn flush? [hand]
  (let [suits-in-hand (map suit hand)
        max-same-suits(apply max(vals(frequencies suits-in-hand)))]
    (if (== max-same-suits 5) true false)))

(defn full-house? [hand]
  (let [ranks-in-hand (map rank hand)
        sorted-vals (sort(vals(frequencies ranks-in-hand)))]
    (if (= sorted-vals (range 2 4)) true false)))

(defn two-pairs? [hand]
  (let [ranks-in-hand (map rank hand)
        sorted-vals (sort(vals(frequencies ranks-in-hand)))]
    (cond
      (= sorted-vals (seq [1 2 2])) true
      (four-of-a-kind? hand)        true
      :else                         false)))

(defn straight? [hand]
  (let [ranks-in-hand (map rank hand)
        sorted-ranks (sort ranks-in-hand)
        re-sorted-ranks (sort(replace {14 1} sorted-ranks))
        fst-srt (first sorted-ranks)
        snd-srt (first re-sorted-ranks)]

    (cond
      (= sorted-ranks (range fst-srt (+ fst-srt 5)))    true
      (= re-sorted-ranks (range snd-srt (+ snd-srt 5))) true
      :else                                             false)))


(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check-hand (fn [function] ((first function) hand))]
        (apply max(map second(filter check-hand checkers)))))

