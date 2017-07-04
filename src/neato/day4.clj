(ns neato.day4
  (:require [clojure.string :as st]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [trace]]
            [neato.shared :refer [parse-dataset]]))

(def id-regex #"\d+")
(def checksum-regex #"\[(.*?)\]")
(def name-regex #"[^0-9]*")

(defn sort-map
  [coll]
  (into (sorted-map-by
         (fn [k1 k2]
           (compare [(k2 coll) k1]
                    [(k1 coll) k2]))) coll))

(defn keywordize-map [coll]
  (w/keywordize-keys
   (into {} (for [[k v] coll] [(str k) v]))))

;; No need to add parethesis when is only a function without args
;; (->> encrypted-name
;;      frequencies
;;      keywordize-map
;;      .... )
;;
;; Sometimes is a bit more clear when you combine small functions very well known:
;; (->> room-name
;;      frequencies
;;      (map (comp vec reverse))
;;      (sort-by identity descendant-order)
;;      (take 5)
;;      (map last)
;;      (apply str)
;;      (= checksum)))
;;
(defn parse-checksum
  [encrypted-name checksum]
  (->> encrypted-name
       (frequencies)
       (keywordize-map)
       (sort-map)
       (take (count checksum))))

;; Usually functions that end in `?` mean they are predicates (functions that return a Boolean value)
;; In this case, to return a boolean you don't need to add an `if`
(defn real?
  [{e :encrypted-name c :checksum id :sector-id}]
  (if (->> (parse-checksum e c)
           (mapv #(-> % first name))
           (st/join "")
           (compare c)
           (= 0))
    (Integer. id)
    0))

;; You don't need to use `assoc` on just one element, you can do it with multiple
;; `(assoc k1 e1 k2 e2 ... kn en)`
;; also you can just use the hashmap constructor
;; `{:sector-id (re-find ...) :checksum (last ...) :encrypted-name (replace ..)}`
;; Good job using `def` for the regex. I would perhaps keeps the regex close the functiona that uses them instead of at the top of the file.
;; Even maybe put them inside of the function as `let`
(defn parse-string
  [x]
  (-> {}
      (assoc :sector-id (re-find id-regex x))
      (assoc :checksum (last (re-find checksum-regex x)))
      (assoc :encrypted-name (st/replace (re-find name-regex x) #"-" ""))))

;; Why using `mapv` over `map` with just one collection?
;; It looks a bit weird that you are doing `map real?` (expecting to map to boolean) and after you add the values ....
(defn sum-of-IDs
  [coll]
  (->> coll
   (mapv real?)
   (reduce +)))

(defn decrypt
  [letter index alpha]
  (let [a-index (.indexOf alpha (str letter))
        length (count alpha)]
    (when (not= -1 a-index)
      (nth alpha (mod (+ a-index index) length) "Not here"))))

(defn decrypt-room
  [{e :encrypted-name id :sector-id :as room}]
  (if (not= 0 (real? room))
    (let [shift (mod (Integer. id) 26)
          chars (vec (seq e))
          alpha ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                 "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]]
      (assoc {} :name (->> chars
                           (mapv #(decrypt % shift alpha))
                           (st/join ""))
                :sector-id id))
    nil))

;; again `mapv` instead of `map` ... unless you really need a vector....
;; Instead of `parse-dataset` you could use `clojure.string/split-lines`
;; Is ok to do two mapping operations, but if you want composition you could write
;; `(map (comp decrypt-room parse-string))`
;; Avoid using generic names like `parse-string`. In this case is not a big problem because is a private function, but is better if the name of the function give a clear idea of what it does like `parse-string-into-room` or `string->room`

(defn decoy-two
  []
  (->> (parse-dataset "day4data.txt")
       (mapv parse-string)
       (mapv decrypt-room)
       (remove nil?)))

;; again `mapv` instead of `map`
;; `sum-of-ids` could be `(filter is-real?) (map :sector-id) (reduce +)` to describe better your intention and what's going on....
(defn decoy-one
  []
  (->> (parse-dataset "day4data.txt")
       (mapv parse-string)
       (sum-of-IDs)))
