(ns bosc.types.number)

(defn number [v] {:type :number :value v :ns 'bosc.types.number})

(defn + [a b]
  (number (clojure.core/+ (:value a) (:value b))))

(defn - [a b]
  (number (clojure.core/- (:value a) (:value b))))
