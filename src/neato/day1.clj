(ns neato.day1
  (:require [clojure.math.numeric-tower :as m]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.tools.trace :refer [trace]]
            [clojure.set :as st]
            [clojure.string :as string]
            [neato.shared :refer [parse-dataset]]))

(defn- gen-visited
  [from to]
  (let [distance (m/abs (- from to))]
    ;; Instead of repeating `(take distance (iterate ...)` you could just
    ;; decide which function to use `dec` or `inc` and then use it with
    ;; `(take distance (iterate f (f from)))`
    ;; Also this looks very much like a range
    ;; maybe like `(range from to)`
    (if (< to from)
      (take distance (iterate dec (dec from)))
      (take distance (iterate inc (inc from))))))

(defn populate-visited
  [{old-x :x old-y :y} {new-x :x new-y :y}]
  (let [x-dist (- old-x new-x)
        y-dist (- old-y new-y)
        x-values (gen-visited old-x new-x)
        y-values (gen-visited old-y new-y)]
    (if (not= old-x new-x)
      (into #{} (map #(conj {:x % :y old-y}) x-values))
      (into #{} (map #(conj {:x old-x :y %}) y-values)))))

(defn- manhattan-distance
  [{l1 :x r1 :y} {l2 :x r2 :y}]
  (+ (m/abs (- l1 l2))
     (m/abs (- r1 r2))))

;; This implementation is Very hard to read
;; Following the name it should add x1 and y1 to the coordinates but it is updating
;; `:visited` as well ....
(defn- add-to-coordinates
  [x1 y1 {x2 :x y2 :y total-visited :visited :as coord}]
  (let [new-x (+ x1 x2)
        new-y (+ y1 y2)
        new-visited (populate-visited {:x x2 :y y2} {:x new-x :y new-y})
        visited (st/union new-visited total-visited)
        coord (assoc coord :x new-x :y new-y :visited visited)]
    (if-let [hq (some new-visited total-visited)]
      (if (empty? (:hq2 coord))
        (assoc coord :hq2 hq)
        coord)
      coord)))

;; The name `get-direction` would suggest is some kind of query but is modifiying `coord` and adding more points
(defn- get-direction [{:keys [towards value]} coord]
  ;; `Using `case` you would have to repeat `(= towards)` each time
  ;; Also maybe try to find a way to return the point and then
  ;; apply the addition after
  (cond
    (= towards "N") (add-to-coordinates 0 (+ value) coord)
    (= towards "E") (add-to-coordinates (+ value) 0 coord)
    (= towards "S") (add-to-coordinates 0 (- value) coord)
    (= towards "W") (add-to-coordinates (- value) 0 coord)
    ;; If you believe you need an `:else` then either provide
    ;; a value that makes sense or throw an exception
    :else
    "get-direction issue"))

;; This also looks like a `reduce` function
(defn calc-coordinates [data]
  ;; You could use pattern matching here for old
  ;; to get [x & xs]
  (loop [old data coordinates {:x 0 :y 0 :visited #{}}]
    (if (empty? old)
      coordinates
      (recur
       (vec (rest old))
       (get-direction (first old) coordinates)))))

(defn- assign-cardinal-direction [{:keys [direction value] :as data} facing]
  ;; instead of `cond` you could use `case` and do something like
  ;; `(case [direction value]
  ;;     [\R \N] \E
  ;;     [\L \N] \W
  ;;     ....)`
  (cond
    (and (= direction "R")(= facing "N")) (assoc data :towards "E")
    (and (= direction "L")(= facing "N")) (assoc data :towards "W")
    (and (= direction "R")(= facing "E")) (assoc data :towards "S")
    (and (= direction "L")(= facing "E")) (assoc data :towards "N")
    (and (= direction "R")(= facing "S")) (assoc data :towards "W")
    (and (= direction "L")(= facing "S")) (assoc data :towards "E")
    (and (= direction "R")(= facing "W")) (assoc data :towards "N")
    (and (= direction "L")(= facing "W")) (assoc data :towards "S")
    ;; If you believe you need an `:else` then either provide
    ;; a value that makes sense or throw an exception
    :else
    "assign-cardinal-direction issue"))

;; Using custom recursion (i.e. `loop`) is a fantastic tool but first we should make sure there's not already a well known function that does the same thing.
;; Having a function that everybody knows how it works provides lots of benefits
;; In this case it looks like a `reduce` function where you call `assign-cardinal-direction` in each step and collect the results in a list
(defn- populate-cardinal-direction [data]
  ;; To use the pattern (first : rest) in `Clojure`
  ;; you can use [x & xs]
  ;; so you could do
  ;; `(loop [[x & xs] data ....)`
  (loop [old data new [] compass "N"]
    (let [point (-> old first (assign-cardinal-direction compass))]
      ;; `old` can not be empty because `first` will return `nil`
      (if (empty? old)
        new
        (recur
         (vec (rest old))
         (conj new point)
         (:towards point))))))

(defn- populate-map
  [x]
  ;; why not just use `head` and then use characters \L n \R
  (let [d (-> x first str)
        ;; for the value you can also use (-> x (.substring 1) read-string)
        v (->> x (re-find  #"\d+") Integer.)
        point {}]
      ;; both `if` branches do the same thing
      ;; there's no need to create an empty `{}` you can just return `{:direction d :value v}`
    (if (= "L" d)
      (assoc point :direction d :value v)
      (assoc point :direction d :value v))))

;; instead of passing a boolean flag, we could just pass a function that is called at the end, and not _if_ is necessary

;; The fn name _calculate_ is a bit ambiguos ... perhaps a name like "find-distance-to-hq" or "find-first-point-visited-twice" could help to understand better the intent

;; Is a bit hard to tell what the function is doing... _populate-map_ could be called _parse-instructions_ (or something like that)

;; instead of using `as->` you could do
;; (->> (split x ...)
;;      parse-instructions
;;      populate-cardinal-direction
;;      calc-coordinates
;;      fn-that-goes-at-the-end)

(defn calculate
  [data hq1?]
  (as-> data x
    (string/split x #", ")
    (mapv populate-map x)
    (populate-cardinal-direction x)
    (calc-coordinates x)
    (if hq1?
      (manhattan-distance {:x 0 :y 0} x)
      (manhattan-distance {:x 0 :y 0} (:hq2 x)))))

;; This could be in the test. Is not used int the code
(defn headquarters-two
  []
  (let [file "day1data.txt"]
    (-> file
        parse-dataset
        first
        (calculate false))))

;; This could be in the test. Is not used int the code
(defn headquarters-one
  []
  (let [file "day1data.txt"]
    (-> file
        parse-dataset
        first
        (calculate true))))
