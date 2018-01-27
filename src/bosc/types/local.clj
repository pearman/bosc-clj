(ns bosc.types.local)

(defn assoc [s k v]
  (clojure.core/assoc s (:value k) v))
