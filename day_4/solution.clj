(ns day_4.solution
  (:require [clojure.string :as s]))

(def input
  (-> "day_4/input.txt"
      slurp
      (s/split #"\n\n")))

(def maps
  (->> input
    (map
      (fn [s]
        (let [kv-strings (s/split s #"\s")]
          (->> kv-strings
            (map (fn [kv-string]
                   (let [[k v] (s/split kv-string #":")]
                     {(keyword k) v})))
            (reduce merge)))))))

(def required-keys
  [:byr :iyr :eyr :hgt :hcl :ecl :pid])

^{:meta "part 1"}
(count (filter
         (fn [m]
           (every?
             #(contains? m %)
             required-keys))
         maps))

;---

(defmulti validate first)

(defmethod validate :byr
  [[_ v]]
  (as-> v $
    (Integer. $)
    (<= 1920 $ 2002)))

(defmethod validate :iyr
  [[_ v]]
  (as-> v $
    (Integer. $)
    (<= 2010 $ 2020)))

(defmethod validate :eyr
  [[_ v]]
  (as-> v $
    (Integer. $)
    (<= 2020 $ 2030)))

(defmethod validate :hgt
  [[_ v]]
  (letfn [(valid-unit?
            [[_ value unit]]
            (when-let [unit* (and (or (= unit "cm")
                                      (= unit "in"))
                                  unit)]
              [value unit]))
          (valid-value?
            [[value unit]]
            (case unit
              "cm" (as-> value $
                     (Integer. $)
                     (<= 150 $ 193))
              "in" (as-> value $
                     (Integer. $)
                     (<= 59 $ 76))))]
    (boolean
      (some->> v
        (re-find #"(\d+)(\w+)")
        valid-unit?
        valid-value?))))

(defmethod validate :hcl
  [[_ v]]
  (boolean (re-matches #"^#[0-9a-f]{6}$" v)))

(defmethod validate :ecl
  [[_ v]]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
             v))

(defmethod validate :pid
  [[_ v]]
  (boolean (re-matches #"^\d{9}$" v)))

(defmethod validate :cid [_] true)

^{:meta "part 2"}
(count (filter
         (fn [m]
           (every?
             #(and (contains? m %)
                   (every? identity (map validate m)))
             required-keys))
         maps))
