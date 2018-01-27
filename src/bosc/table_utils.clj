(ns bosc.table-utils
  (:require [clojure.walk]))

(defn seq->table-arr
  ([t s] (-> (zipmap
               (map #(->> % str keyword) (range (count s)))
               s)
             (assoc :type t :length (count s))))
  ([s] (seq->table-arr :list s)))


(defn seq->map
  [s]
  (let [result (->> (partition 2 s)
                    (map (fn [s] [(-> s first :value) (last s)]))
                    (into {}))]
    (assoc result :type :map)))

(defn get-index [table index]
  ((keyword (str index)) table))

(defn table->seq [table]
  (let [length (get table :length)
        keys (map #(keyword (str %)) (range length))
        objs (map #(get table %) keys)]
    objs))
