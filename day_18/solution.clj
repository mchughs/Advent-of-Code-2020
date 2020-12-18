(ns day_18.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as ss]
            [utils :as u]))

(def input
  (-> "day_18/input.txt"
    slurp
    s/split-lines))

(def with-parens #"\((\d+) ([+*]) (\d+)\)")
(def without-parens #"(\d+) ([+*]) (\d+)")

(defn operate
  [[_ x op y]]
  (let [op' (case op "+" + "*" *)]
    (op' (Long. x)
         (Long. y))))

(defn evaluate [expr]
  (loop [expr expr]
    (if-let [parens-expr (re-find with-parens expr)]
      (recur (s/replace-first expr with-parens (str (operate parens-expr))))
      (if-let [nonparens-expr (re-find without-parens expr)]
        (recur (s/replace-first expr without-parens (str (operate nonparens-expr))))
        (Long. expr)))))

^{:meta "part1"}
(->> input
  (map evaluate)
  (reduce +))

;---

(def with-parens-plus #"\((\d+) \+ (\d+)\)")
(def with-parens-mult #"\(\d+(?: \* \d+)+\)")
(def without-parens-plus #"(\d+) \+ (\d+)")
(def without-parens-mult #"(\d+) \* (\d+)")

(defn operate-plus [[_ x y]]
  (+ (Long. x) (Long. y)))

(defn operate-mult [nums]
  (reduce
    #(* %1 (Long. %2))
    1
    nums))

(defn evaluate* [expr]
  (prn expr)
  (loop [expr expr]
    (if-let [parens-expr-plus (re-find with-parens-plus expr)]
      (recur (s/replace-first expr with-parens-plus (str (operate-plus parens-expr-plus))))
      (if-let [parens-expr-mult (re-find with-parens-mult expr)]
        (recur (s/replace-first expr with-parens-mult (str (operate-mult (re-seq #"\d+" parens-expr-mult)))))
        (if-let [nonparens-expr-plus (re-find without-parens-plus expr)]
          (recur (s/replace-first expr without-parens-plus (str (operate-plus nonparens-expr-plus))))
          (if-let [nonparens-expr-mult (re-find without-parens-mult expr)]
            (recur (s/replace-first expr without-parens-mult (str (operate-mult nonparens-expr-mult))))
            (Long. expr)))))))

^{:meta "part2"}
(->> input
  (map evaluate*)
  (reduce + 0))
