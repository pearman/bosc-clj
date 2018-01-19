(ns bosc.types.local)

(defn assoc [s k v]
  (swap! s clojure.core/assoc (:value k) v))


