(ns bosc.interpreter
  (:require [bosc.parser :as parser]
            [bosc.table-utils :as utils]))

(defn get-index [table index]
  ((keyword (str index)) table))

(defn resolve-method [root m]
  (let [type (:type m)
        value (:value m)]
    (cond
      (= type :symbol)
      (let [fun (ns-resolve (:ns root) (symbol (name value)))
            args (assoc
                  (utils/seq->table-arr (first (:arglists (meta fun))))
                  :type :list)]
        {:type :method :args args :native-method fun}))))

(defn execute [statement]
  (println "executing")
  (apply
   (get-in statement [:method :native-method])
   (:args statement)))

(defn accum-statement [sym statement]
  (let [result
        (cond
          (nil? (:root statement))
          (assoc statement :root sym :args [sym])

          (nil? (:method statement))
          (assoc statement :method (resolve-method (:root statement) sym))

          (> (get-in statement [:method :args :length] 0)
             (count (:args statement)))
          (assoc statement :args (conj (:args statement) sym)))]
    (println result)
    (if (= (get-in result [:method :args :length] -1)
           (count (:args result)))
      (let [executed (execute result)]
        {:root executed :args [executed]})
      result)))

(defn table-eval [table]
  (loop [index 0
         sym (get-index table index)
         statement {}]
    (if (nil? sym)
      (:root statement)
      (let [next-index (inc index)]
        (recur
         next-index
         (get-index table next-index)
         (accum-statement sym statement))))))

(defn eval [str]
  (->> str
       parser/parse
       table-eval))

(eval "{:cool 5 :neat 10}")
;(parse "$(fun x)")
