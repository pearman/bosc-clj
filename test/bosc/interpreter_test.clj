(ns bosc.interpreter-test
  (:require [clojure.test :refer :all]
            [bosc.interpreter :refer :all]))

(deftest basic-execution
  (testing "basic statement contruction and execution"
    (is (=
         (eval "1 + 2")
         {:type :number, :value 3, :ns 'bosc.types.number}))
    (is (=
         (eval "1 + (2 + 5)")
         {:type :number, :value 8, :ns 'bosc.types.number}))
    (is (=
         (eval "(1 + 2) + 8")
         {:type :number, :value 11, :ns 'bosc.types.number}))))

(deftest global-resolution
  (testing "testing global resolution"
    (is (=
         (eval "global assoc :x 3 | x")
         {:type :number, :value 3, :ns 'bosc.types.number}))
    (is (=
         (eval "x + 1")
         {:type :number, :value 4, :ns 'bosc.types.number}))))

(deftest local-resolution
  (testing "testing global resolution"
    (is (=
         (eval "local assoc :y 3 | y")
         {:type :number, :value 3, :ns 'bosc.types.number}))
    (is (=
         (eval "local assoc :y 3 | y + 1")
         {:type :number, :value 4, :ns 'bosc.types.number}))))
