(ns bosc.interpreter
  (:require [bosc.parser :as parser]
            [bosc.table-utils :as utils]
            [bosc.types.global :as global-ns]
            [bosc.types.local :as local-ns]
            [bosc.types.table :as table-ns]
            [clojure.inspector :as inspector]))

(declare table-eval execute)

(def global (atom {:type :global :ns 'bosc.types.global}))
(defn new-local [] {:type :local :ns 'bosc.types.local})


(defn resolve-native-method [root m local]
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
        {:type :method :args args :native-method fun})
      :else m)))

(defn resolve-symbol [sym local]
  (cond
    (= (:value sym) :global) global

    (= (:value sym) :local) local

    :else
    (or ((:value sym) local)
        ((:value sym) @global)
        sym)))

(defn resolve [sym local]
  (let [type (:type sym)
        value (:value sym)]
    (case type
      :execute (table-eval sym local)
      :prefix-function (let [call (utils/table->seq sym)
                             method (resolve (first call) local)
                             statement {:root nil
                                        :method method
                                        :args (rest call)}]
                         (execute statement local))
      :infix-function (table-eval sym local)
      :symbol (resolve-symbol sym local)
      sym)))

(defn build-args [method args]
  (let [method-args (utils/table->seq (get method :args))
        arg-syms (map :value method-args)]
    ;;(clojure.pprint/pprint (zipmap arg-syms args))
    (zipmap arg-syms args)))

(defn execute [statement local]
  ;;(clojure.pprint/pprint statement)
  (let [method (get statement :method)
        native-method (get method :native-method)
        args (:args statement)]
    (if (nil? native-method)
      (table-eval
       method
       (build-args method args))
      (apply
       native-method
       args))))

(defn accum-statement [symIn statement local]
  (let [sym (resolve symIn local)
        result
        (cond
          (nil? (:root statement))
          (assoc statement :root sym :args [sym])

          (nil? (:method statement))
          (assoc statement
                 :method
                 (resolve-native-method (:root statement) sym local))

          (> (get-in statement [:method :args :length] 0)
             (count (:args statement)))
          (assoc statement :args (conj (:args statement) sym)))]
    ;;(clojure.pprint/pprint statement)
    (if (= (get-in result [:method :args :length] -1)
           (count (:args result)))
      (let [executed (execute result local)]
        ;;(clojure.pprint/pprint executed) 
        {:root executed :args [executed]})
      result)))

(defn table-eval [table local]
  (loop [index 0
         sym (utils/get-index table index)
         statement {}]
    (if (nil? sym)
      (:root statement)
      (let [next-index (inc index)
            new-local (if (= (get-in statement [:root :type]) :local)
                        (:root statement)
                        local)]
        (recur
         next-index
         (utils/get-index table next-index)
         (accum-statement sym statement new-local))))))

(defn eval [str]
  (-> str
      parser/parse
      (table-eval (new-local))))

;(eval "local assoc :x 10 | x + 10")
;(parse "$(fun x)")


