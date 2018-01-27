(ns bosc.types.global)

(defn assoc [s k v]
  (swap! s clojure.core/assoc (:value k) v))
