^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns clojure-intro
  {:nextjournal.clerk/toc :collapsed})

;; # Quick intro to Clojure
;;
;; This document is aimed at people with no Clojure experience who are
;; familiar with general programming concepts,
;; so they can follow the code in my other notebooks.
;;
;; Here are some basic differences between Python and Clojure:
;;
;; ## Python
;;
;; ```py
;; def strange_sum(a, b, c):
;;     d = b - a
;;     if d > 0:
;;         return a + c
;;     else:
;;         return b + c
;;
;; r = strange_sum(3, 4, 5)
;;
;; 1 + 2 * 3 + 4        # 11
;; (1 + 2) * (3 + 4)    # 21
;;
;; lambda x: 2*x
;;
;;
;; ```
;;
;; - functions are defined with `def`
;; - function parameters are separated by commas
;; - local variable (`d`) is just declared normally inside of a function
;; - `+` is an infix operator
;; - functions are called with `func_name(arg1, arg2, ...)`
;; - we need to be careful about operator precedence: `*` before `+`
;; - anonymous functions are defined with `lambda`
;;
;;
;;
;; ## Clojure
;;
;; The same code in Clojure looks as follows:
;;
(defn strange-sum [a b c]
  (let [d (- b a)]
    (if (pos? d)
      (+ a c)
      (+ b c))))

;; > Oh, so many (closing) parentheses, how do you keep track of that?
;;
;; You don't. Your code editor does that automatically for you when
;; you're writing the code.
;; And when you're reading the code, you learn to ignore them.\
;; Pro tip: just focus on the indentation.


;; The main differences compared to the Python code are:
;; - we put a parenthesis _before_ everything
;; - we use `defn` to define functions
;; - parameters are in a square bracket, separated by whitespace
;;   (commas are treated as whitespace,
;;   i.e. `[a b c]` is the same as `[a, b ,,,,, c]`)
;; - local variables are declared using `let` and they stay in scope
;;   until the closing parenthesis of the `let`
;; - `if` _expression_ consists of three parts:
;;   `(if <clause> <then_branch> <else_branch>)`
;; - there are no infix operators, `+` is just a regular function
;; - when calling a function, its name is _inside_ of parentheses, always at
;;   the first position; the rest of elements inside parenthesis are function
;;   arguments, i.e. `func_name(a, b)` becomes `(func-name a b)`
;; - to declare and define a _global_ variable, we use `def` (`=` is not used):
;;
(def r (strange-sum 3 4 5))


;; - `+` and `*` are just a regular functions, there's no precedence, the order
;;   of the operations is defined by the way they're written and nested:
;;
(+ 1 (* 2 3) 4)

(* (+ 1 2)
   (+ 3 4))


;; - anonymous functions are defined with `fn` or by using the special
;;   syntax `#(... % ...)`, where `%` is the parameter we're passing in:
;;
(fn [x] (* 2 x)) ; the same as #(* 2 %)





;; ## Datatypes
;;
;; ### Lists
;;
;; Everything enclosed in parenthesis is a `list`, even those function calls
;; mentioned above (where the function name is always the first element of a list).
;; To create a list (and not call/evaluate a function), we need to either
;; quote it with `'` or use `list` function:

(def my-list '(1 2 3 4))
(def also-list (list 1 2 3 4))

;; NOTE: Since there is no `var_name = var_value` declarations in Clojure,
;; to compare two values we use `=` function (no need for `==`).
;; The function is, as always, in the first place in a list.
(= my-list also-list)

;; Lists are implemented as linked lists, meaning that new elements are
;; added at the beginning of a list, and accessing a random element is
;; not efficient (`O(n)`).
;;
(conj my-list 99)



;; ### Vectors
;;
;; Vectors are also ordered collections of elements, similar to lists,
;; but they are implemented as arrays: new elements are appended at the back,
;; and they have efficient (`O(log n)`) random accessing.\
;; Vectors are enclosed in square brackets.
;;
(def my-vec [2 3 4 5 6 7])

(conj my-vec 99)

;; We can convert a list to a vector by using `vec` function:
(vec my-list)



;; ### Maps (dictionaries)
;;
;; Maps are unordered collections of key-value pairs.
;; They are enclosed in `{ ... }`.
;; There are no `,` or `:` between the elements of a map,
;; we just have to have an even number of elements.
;;
(def my-map {"foo" 2
             "bar" 3})



;; ### Keywords
;;
;; While we can use strings as keys in a map, a more idiomatic (and efficient)
;; way is to use keywords.
;; Keywords have `:` in front of their name.
;;
{:foo 2
 :bar 3}


;; To convert a collection of key-value pairs to a map, use `into {}`:
;;
(into {} [[:foo 7] [:bar 8]])




;; ### Sets
;;
;; Sets are collections with unique values, enclosed in `#{ ... }`

(def my-set #{1 2 3 4})

;; To convert other collections to a set, we use `set` function.
(set [1 2 2 2 3 4 4 4 4])







;; ## Functions
;;
;; With Clojure being a functional language, the key part in using it is knowing
;; the functions available in the standard library.
;; We transform our data by applying multiple functions to it.

;; ### Functions on numbers

(inc 5)
(dec 9)
(max 3 2 5 1 4)
(mod 19 5)
(odd? 3)


;; ### Functions on strings

(str "We" "Can" "Concatenate" " strings")
(parse-long "2231")


;; ### Functions on collections

my-vec

;; To add an element to a list or vector, use `conj` (short for "conjoin"):
;;
(conj my-vec 999)

;; Clojure is immutable: `conj`-ing to `my-vec` creates a new vector with
;; the new element, while `my-vec` stays the same:
my-vec

(first my-vec)
(rest my-vec)


(take 2 my-vec)
(drop 2 my-vec)

(take-while (fn [x] (< x 5)) my-vec)
(drop-while (fn [x] (< x 5)) my-vec)

;; To change a value of an existing key or add a new key-value pair in a map,
;; use `assoc`.
;;
(assoc my-map :new-key :new-val)
(assoc my-map "foo" 3333)

;; You can also `update` a value of a key:
;;
(update my-map "bar" (fn [x] (* 100 x)))


;; Once again, Clojure is immutable, so the `my-map` is still the same as
;; when we initially declared it:
;;
my-map



;; ## Functional functions
;;
;; Functional languages usually come with a typical set of functions that work
;; on collections.
;; Here are the most frequently used ones.
;;
;; ### map
;; To apply a function to each element of a collection, we use the `map` function:

(map inc my-vec)

(defn double-it [x]
  (* 2 x))

(map double-it my-vec)

;; If you want to return a vector after applying a function to each element, use `mapv`:

(mapv (fn [x] (* 3 x)) my-vec)


;; ### filter
;; To keep only the elements of a collection which satisfy a predicate.

(filter odd? my-vec)

;; Similarly to `mapv`, to return a vector, use `filterv`:

(filterv (fn [x] (> x 4)) my-vec)


;; ### reduce
;; With a `reduce` our aim is to "accumulate" a collection: the result of a reduce
;; is a single value.

(reduce + my-vec)


;; You can think of `reduce` as an equivalent of running a for-loop through a collection
;; where each element changes the accumulator in some way:
;;
;; ```py
;; acc = 0
;; for x in [10, 20, 30, 40]:
;;     acc += 5*x
;; ```
;;
;; ```py
;; # or:
;; sum(5*x for x in [10, 20, 30, 40])
;; ```

;; Clojure equivalent:
(reduce (fn [acc x]
          (+ acc (* 5 x)))
        0
        [10 20 30 40])

(reduce + (map #(* 5 %) [10 20 30 40]))


;; The first argument to the reduce is a function which should take two arguments
;; (an accumulator and a current element).
;; If an initial value is not supplied,
;; the function is called on first two elements of a collection.
;; The next invocation of the function is called on the result of a previous call
;; and the third element.
;; Rinse and repeat until the end of the collection.
;;
;; We can see it in action, step by step, by using the `reductions` function:
(reductions + my-vec)

;; We can also supply an initial value:
(reduce + 100 my-vec)
(reductions + 100 my-vec)




;; ## Function chaining
;;
(reduce * (take 2 (map double-it my-vec)))

;; We can write the same thing by using `->>`:

(->> (map double-it my-vec)
     (take 2)
     (reduce *))

;; There is also `->`.
;; The difference between `->` and `->>` is where the result of a previous
;; evaluation is placed in the new function.
;; `->` puts the result as a first argument, while `->>` puts it as last.
;;
(-> "a"
    (str "b")   ; doing (str "a" "b")
    (str "c"))

(->> "a"
     (str "b")  ; doing (str "b" "a")
     (str "c"))





;; ## Conclusion
;;
;; The most important thing to start understanding the Clojure code is that
;; stuff enclosed in `( ... )` are function calls and the first element of
;; those lists is a function we are calling and the rest are the arguments
;; for that function.
;; (Those arguments can also be function calls, with their arguments,
;; which can also be funct——, etc etc.)
;;
;; To see what each function does, search for it in the
;; [Clojure Cheat Sheet](https://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-cdocs-summary.html),
;; the documentation for each function has both a docstring and several
;; examples to showcase its behaviour.
;;
;; ----
;;
;; ----
;;
;; > Well, actually...
;;
;; Yes, I've made several simplifications throughout this guide so some things
;; are not entirely technically correct.
;; This was done on purpose to not go into minutiae, unimportant for people
;; seeing Clojure code for the first time.
