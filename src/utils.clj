(ns utils
  (:gen-class))

(defn find-first
  [f coll]
  (->> coll
       (filter f)
       first))

(defn xor [a b]
  (or (and a (not b))
      (and b (not a))))

(defn update-all [f m]
  (into {}
    (for [[k v] m]
      {k (f v)})))
