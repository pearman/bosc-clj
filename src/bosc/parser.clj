(ns bosc.parser
  (:require [instaparse.core :as insta]
            [bosc.table-utils :as table-utils]
            [bosc.types.number]))

(def parser
  (insta/parser
   " <prog> = <whitespace>* expr (<whitespace> expr)* <whitespace>*
      <expr> = boolean / nil / symbol / keyword / number
                / string / execute / prefix-function
                / infix-function / list / map / method
                / <comment>
      comment = ';' #'.*' (#'\n' | #'$')
      execute = <'('> prog <')'>
      prefix-function = <'$('> prog <')'>
      method = <'#('> prog <')'>
      infix-function = <'`'> prog <'`'>
      list = <'['> prog <']'>
      map = <'{'> prog <'}'>
      symbol = #'[+\\-*/,!<>=%a-zA-Z_-][=a-zA-Z0-9_-]*'
      keyword = <':'> symbol
      number = #'-?(0|[1-9][0-9]*)([.][0-9]+)?'
      string = <'\\\"'> (#'[^\"\\\\]+' | escaped-char)* <'\\\"'>
      <escaped-char> = #'\\\\.'
      boolean = 'true' | 'false'
      nil = 'nil'
      whitespace = #'\\s+' "))

(def transform-options
  (let [label-fn (fn [type transform ns & value]
                   (let [base-map (if (and
                                       (= (count value) 1)
                                       (not= type :list))
                                    {:type type :value (transform (first value))}
                                    {:type type :value (map transform value)})]
                     (if (nil? ns)
                       base-map
                       (assoc base-map :ns ns))))
        label #(partial label-fn %1 %2 nil)
        label-ident #(partial label-fn %1 identity nil)
        label-ns #(partial label-fn %1 %2 %3)]
    {:number (label-ns :number read-string 'bosc.types.number)
     :boolean (label :boolean read-string)
     :symbol (label :symbol keyword)
     :keyword (label :keyword (fn [x] (:value x)))
     :nil (label :nil (fn [x] nil))
     :string (label-ident :string)
     :execute (fn [& m] (merge {:type :execute} (table-utils/seq->table-arr m)))
     :prefix-function (fn [& m] (merge  (table-utils/seq->table-arr m) {:type :prefix-function}))
     :infix-function (fn [& m] (merge (table-utils/seq->table-arr m)  {:type :infix-function}))
     :method (fn [& m] {:type :method :args (first m) :value (table-utils/seq->table-arr (rest m))})
     :list (fn [& m] (merge  (table-utils/seq->table-arr m) {:type :list}))
     :map (fn [& m] (merge (table-utils/seq->map m)  {:type :map}))}))

(defn print-and-return [x]
  (println x)
  x)

(defn root->list [r]
  {:type :list :value r})

(defn parse [input]
  (->> (parser input)
       (insta/transform transform-options)
       (table-utils/seq->table-arr :execute)))

;(parse "$(fun x)")

;(parse "[1 2 {:x 5}]")
