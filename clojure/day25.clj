^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day16
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require
   aoc
   [clojure.pprint :as pp]
   [nextjournal.clerk :as clerk]))


(defn pstr [data] (with-out-str (pp/pprint data)))

;; # Day 25: Code Chronicle
;;
(def example "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")


;; ## Input parsing
(defn grid-type [grid]
  (let [row-vals (for [[[x y] c] grid] [y c])
        by-row (group-by first row-vals)
        row-value-sets (update-vals by-row #(set (map second %)))
        last-row (apply max (keys by-row))]
    (cond
      (= (row-value-sets 0) #{\#}) :lock
      (= (row-value-sets last-row) #{\#}) :key)))

(defn lock-pin-height [pin-col]
  (->> pin-col
       (map second)
       (partition-by identity)
       first
       count
       dec))

(defn lock-heights [grid]
  (->> (sort grid)
       (partition-by (fn [[[x y] c]] x))
       (map lock-pin-height)))

(defn key-pin-height [pin-col]
  (->> pin-col
       (map second)
       (partition-by identity)
       last
       count
       dec))

(defn key-heights [grid]
  (->> (sort grid)
       (partition-by (fn [[[x y] c]] x))
       (map key-pin-height)))

(defn parse-data [input]
  (let [grids (into [] (for [lines (aoc/parse-paragraphs input)]
                         (aoc/grid->point-map lines)))
        typed (group-by grid-type grids)]
    {:locks (map lock-heights (:lock typed))
     :keys (map key-heights (:key typed))}))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 25)))



;; ## Part 1

(defn fit? [lock key]
  (let [m (apply max (map + lock key))]
    (<= m 5)))

(defn part-1 [{:keys [locks keys]}]
  (->> (for [lock locks
             key keys
            :when (fit? lock key)]
         true)
       (count)))

(part-1 example-data)
(part-1 data)


;; ## Part 2
(defn part-2 [data]
  (part-1 data))


(part-2 example-data)
;(part-2 data)


^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
