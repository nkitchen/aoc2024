(ns solutions-tests
  (:require
   day01 day02 day03 day04 ;day05
   ;; day06 day07 day08 day09 day10
   ;; day11 day12 day13 day14 day15
   ;; day16 day17 day18 day19 day20
   ;; day21 day22 day23 day24 day25
   [clojure.test :refer [deftest is run-tests successful?]]))



(defmacro check-day [day test-results real-results]
  (let [day        (format "%02d" day)
        full-day   (str "day" day)
        main-func  (symbol full-day "-main")
        test-name  (symbol (str full-day "-test"))
        test-input (str day "_test")]
    `(deftest ~test-name
       (when ~test-results
         (is (= ~test-results (~main-func (aoc/read-input ~test-input)))))
       (is (= ~real-results (~main-func (aoc/read-input ~day)))))))




(check-day 1 [11 31] [1222801 22545250])
(check-day 2 [2 4] [334 400])
(check-day 3 [161 48] [167090022 89823704])
(check-day 4 [18 9] [2534 1866])


(let [summary (run-tests)]
  (when-not (successful? summary)
    (throw (Exception. "tests failed"))))
