(ns basics.core
  (:require [clojure.math.numeric-tower :as math]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn sum [xx]
  "Take the sum of a collection"
  (apply + xx))

(defn prod [xx]
  "Takes the product of a collection"
  (apply *' xx))

(defn sqrt [n]
  "Find the square root"
  (math/sqrt n))

(defn square [n]
  "Squares n"
  (* n n))

(defn cube [n]
  "Cubes n"
  (* n n n))

(defn expo [a n]
  "Takes a to the power of n"
  (prod (repeat n a)))

(defn quotient [xx]
  "Takes the quotient of a collection"
  (apply / xx))

(defn diff [xx]
  "Finds the difference of a collection, subtracting the remaining parts from the first"
  (apply - xx))

(def fibo
  "The fibonacci sequence"
  (lazy-cat [0 1] (map + fibo (rest fibo))))

(defn divisors [n]
  "Find all the divisors of n, returns a vector and includes n"
  (conj
    (filterv
      (comp zero? (partial rem n))
        (range 1 (inc (/ n 2)))) n))

(defn divisors-no-n [n]
  "Find all the divisors of n, returns a vector and does not have n at the end"
  (filterv
    (comp zero? (partial rem n))
      (range 1 (inc (/ n 2)))))

(def certainty 5)

(defn prime? [n]
  "Checks if n is prime or not"
  (.isProbablePrime (BigInteger/valueOf n) certainty))

(defn triangle-number [n]
  "Finds the nth triangle-number"
  (sum (range (inc n))))

(def triangles
  "A list of triangle numbers"
  (map triangle-number (iterate inc 1)))

(defn fact [n]
  "Take the factorial of n"
  (if (= n 1) 1
    (*' n (fact (dec n)))))

(defn mean [xx]
  "Find the mean of a set"
  (/ (sum xx) (count xx)))

(defn prob-map [xx]
  "Creates a probability-map for a discrete set"
  (let [total (count xx)]
    (into {}
      (map
        (fn [k]
          [(keyword (str k))
          (/ (count (filter #( = % k) xx)) total)])
           xx))))

(defn pmf [xx n]
  "Find the probability of n in collection"
  (get (prob-map xx) (keyword (str n))))

(defn expected-value [xx]
  "Find the expected value of a collection"
  (sum
    (map
      #(double (* (pmf xx %) %))
        (into #{} xx))))

(defn variance [xx]
  "Variance of a set"
  (-
    (expected-value (mapv square xx))
    (square (expected-value xx))))

(defn std [xx]
  "Find the standard deviation of a set"
  (sqrt (variance xx)))
