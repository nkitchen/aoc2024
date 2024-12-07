^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day07
  {:nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc :collapsed}
  (:require aoc))


;; # Day 7: Bridge Repair
;;
;; We're standing next to a bridge and we need to help a group of engineers
;; with their calibrations: some young elephants stole their operators, so
;; they are left with a list of numbers that looks like this:
;;
(def example "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")




;; ## Input parsing
;;
;; We'll extract all numbers from each line, knowing that the first value is
;; a result of an equation, and the remaining numbers are operands:
;;
(defn parse-data [input]
  (aoc/parse-lines input :ints))

(def example-equations (parse-data example))
(def equations (parse-data (aoc/read-input 7)))




;; ## Solution
;;
;; Since both parts are basically the same, we will have one set of functions
;; to solve both.
;;
;; There are three possible operations:
;; - addition (`2 + 3 = 5`)
;; - multiplication (`2 * 3 = 6`)
;; - concatenation (`2 || 3 = 23`) (only for part 2)
;;
;; The first two are, of course, built-ins. We can define the third as:
;;
(def || (comp parse-long str))



;; For each equation we will recursively check if it is valid:
;; - If the equation contains just one operand, the equation is valid if
;;   that operand is equal to the result.
;; - If the first operand is already higher than the result, the equation
;;   can never be valid (each operation only increases the final value).
;; - Otherwise, we need to try all operations on the first two operands
;;   and recurse with the result of that operation as a first operand.\
;;   e.g. `[a b c d e] --> [(operation a b) c d e]`
;;
(defn valid-equation? [res [a b & tail] concat-op?]
  (let [valid-op? (fn [op]
                    (valid-equation? res (conj tail (op a b)) concat-op?))]
    (cond
      (nil? b)  (when (= res a) res)
      (> a res) nil
      :else (or (valid-op? +)
                (valid-op? *)
                (when concat-op? (valid-op? ||))))))


;; The only difference between part 1 and part 2 is if we use the concatenation
;; operator or not:
;;
(defn valid-1? [[res & ops]]
  (valid-equation? res ops false))

(defn valid-2? [[res & ops]]
  (valid-equation? res ops true))


;; Now we're ready to solve both parts.
;;
;; One thing to notice is that if an equation is valid in the first part, then
;; it will be also valid in the second part (we can use exactly the same operands
;; as in part 1) and there's no need to check it again,
;; especially since finding suitable operators in part 2 is significantly slower,
;; e.g. the longest equations have 11 operations:
;; - part 1: `2^11 = 2048` potential operations to check
;; - part 2: `3^11 = 177147` potential operations, 86 times more
;;
;; The [`group-by` function](https://clojuredocs.org/clojure.core/group-by)
;; will be useful once again: we'll split the equations in those that
;; satisfy the `valid-1?` functions and those that don't.\
;; To calculate the result for part 1, it is enough to sum all the results
;; (the first element of the list) from the first group.
;;
;; For the second part, we need to check the failing equations if they
;; satisfy the `valid-2?` function (once again we'll use the
;; [`pmap` function](https://clojuredocs.org/clojure.core/pmap) to do that
;; in parallel and gain some performance)
;; and calculate the sum of the results of those which do.
;; In the end, we need to add the result from part 1.
;;
(defn solve [equations]
  (let [{pt1-eqs true
         pt2-eqs false} (group-by (comp some? valid-1?) equations)
        pt1 (aoc/sum-map first pt1-eqs)
        pt2 (->> pt2-eqs
                 (pmap valid-2?)
                 (filter some?)
                 (reduce +)
                 (+ pt1))]
    [pt1 pt2]))

(solve example-equations)
(solve equations)





;; ## Conclusion
;;
;; Today felt like a natural task to use recursion:
;; we have two simple exit conditions and we know how to get there (by trying
;; all possible combinations of operations).
;; A perfect job for the _Recursion Fairy_:
;;
;; > Your only task is to simplify the original problem, or to solve it directly
;; > when simplification is either unnecessary or impossible;
;; > the Recursion Fairy will magically take care of all the simpler
;; > subproblems for you.
;; >
;; > — Jeff Erickson, Algorithms
;;
;;
;; Today's highlights:
;; - `[a b & tail]`: unpack a seqence into its first two elements and all the rest









^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (solve (parse-data input)))
