(ns bosc.table-utils
  (:require [clojure.walk]))

(defn squash-value [m]
  (let [preserve-value? (nil?
                          (some
                            #{(:type m)}
                            [:execute :list :method
                             :infix-function :prefix-function]))]
    (if preserve-value?
      m
      (merge (dissoc m :value) (:value m)))))

(defmulti ast->table #(cond
                        (map? %) :map
                        (seq? %) :seq
                        (vector? %) :seq
                        :else :default))

(defmethod ast->table :seq [s]
  (-> (zipmap
         (map #(->> % str keyword) (range (count s)))
         (map ast->table s))
      (assoc :length (count s))))

(defmethod ast->table :map [m]
  (let [result (clojure.walk/postwalk
                 (fn [node]
                   (cond
                    (seq? node) (ast->table node)
                    :else (squash-value node)))
                 m)]
    (squash-value result)))

(defmethod ast->table :default [x] x)
