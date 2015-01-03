(ns simple.core)

(defprotocol Base
  (inspect [this]))
(defprotocol Reducible
  (reducible? [this])
  (sreduce [this]))

(defrecord SNumber [value]
  Object
  (toString [_] (str value))
  Base
  (inspect [this] (str "«" (.toString this) "»"))
  Reducible
  (reducible? [_] false)
  (sreduce [this] this))

(defrecord Add [left right]
  Object
  (toString [_] (str (.toString left) " + " (.toString right)))
  Base
  (inspect [this] (str "«" (.toString this) "»"))
  Reducible
  (reducible? [_] true)
  (sreduce [_]
    (cond
      (reducible? left) (->Add (sreduce left) right)
      (reducible? right) (->Add left (sreduce right))
      :else (->SNumber (+ (.value left) (.value right))))))

(defrecord Multiply [left right]
  Object
  (toString [_] (str (.toString left) " * " (.toString right)))
  Base
  (inspect [this] (str "«" (.toString this) "»"))
  Reducible
  (reducible? [_] true)
  (sreduce [_]
    (cond
      (reducible? left) (->Add (sreduce left) right)
      (reducible? right) (->Add left (sreduce right))
      :else (->SNumber (* (.value left) (.value right))))))

(defn run [expression]
  (cons expression
        (lazy-seq (map sreduce
                       (take-while reducible? (iterate sreduce expression))))))
