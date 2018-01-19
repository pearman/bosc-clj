(ns bosc.interpreter
  (:require [bosc.parser :as parser]
            [bosc.table-utils :as utils]
            [bosc.types.local :as local-ns]
            [bosc.types.table :as table-ns]
            [clojure.inspector :as inspector]))

(declare table-eval)

(def local (atom {:type :local :ns 'bosc.types.local}))

(defn get-index [table index]
  ((keyword (str index)) table))

(defn resolve-method [root m]
  (let [root (if (inspector/atom? root) @root root)
        type (:type m)
        value (:value m)]
    (cond
      (= type :symbol)
      (let [fun (or (ns-resolve (:ns root) (symbol (name value)))
                    (ns-resolve 'bosc.types.table (symbol (name value))))
            args (assoc
                  (utils/seq->table-arr (first (:arglists (meta fun))))
                  :type :list)]
        {:type :method :args args :native-method fun}))))

(defn resolve-symbol [sym]
  (if (= (:value sym) :local)
    local
    (or ((:value sym) @local) sym)))

(defn resolve [sym]
  (let [type (:type sym)
        value (:value sym)]
    (case type
      :execute (bosc.interpreter/table-eval sym)
      :symbol (resolve-symbol sym)
      sym)))

(defn execute [statement]
  (apply
   (get-in statement [:method :native-method])
   (:args statement)))

(defn accum-statement [symIn statement]
  (let [sym (resolve symIn)
        result
        (cond
          (nil? (:root statement))
          (assoc statement :root sym :args [sym])

          (nil? (:method statement))
          (assoc statement :method (resolve-method (:root statement) sym))

          (> (get-in statement [:method :args :length] 0)
             (count (:args statement)))
          (assoc statement :args (conj (:args statement) sym)))]
    ;(println "--------------")
    ;(println result)
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

;(eval "local assoc :x 10 | x + 10")
;(parse "$(fun x)")



