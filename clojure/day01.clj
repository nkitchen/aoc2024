^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day01
  {:nextjournal.clerk/auto-expand-results? true}
  (:require aoc))


;; # Day 01: .....
;;
;; ## Input parsing
;;
(def input (aoc/read-input 1))

(def data (aoc/parse-lines input :int))


;; ## Part 1
;;
(def pt1 (aoc/count-if odd? data))



;; ## Part 2
;;
(def pt2 (aoc/find-first #(> % 40) data))



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [_]
  [pt1 pt2])
