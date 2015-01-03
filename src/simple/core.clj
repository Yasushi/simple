(ns simple.core)

(defmulti s-inspect "inspect expression" class)
(defmethod s-inspect :default [this] (str "«" (.toString this) "»"))

(defmulti s-reducible? "" class)
(defmethod s-reducible? :default [this] false)

(defmulti s-reduce "" class)

(defrecord SNumber [value]
  Object
  (toString [_] (str value)))
(defmethod s-reducible? SNumber [_] false)
(defmethod s-reduce SNumber [this] this)

(defrecord SAdd [left right]
  Object
  (toString [_] (str (.toString left) " + " (.toString right))))
(defmethod s-reducible? SAdd [_] true)
(defmethod s-reduce SAdd [{:keys [left right]}]
  (cond
    (s-reducible? left) (->SAdd (s-reduce left) right)
    (s-reducible? right) (->SAdd left (s-reduce right))
    :else (->SNumber (+ (.value left) (.value right)))))

(defrecord SMultiply [left right]
  Object
  (toString [_] (str (.toString left) " * " (.toString right))))
(defmethod s-reducible? SMultiply [_] true)
(defmethod s-reduce SMultiply [{:keys [left right]}]
  (cond
    (s-reducible? left) (->SMultiply (s-reduce left) right)
    (s-reducible? right) (->SMultiply left (s-reduce right))
    :else (->SNumber (* (.value left) (.value right)))))

(defn s-run [expression]
  (cons expression
        (lazy-seq (map s-reduce
                       (take-while s-reducible? (iterate s-reduce expression))))))

(defrecord SBoolean [value]
  Object
  (toString [_] (str value)))
(defmethod s-reducible? SBoolean [_] false)
(defmethod s-reduce SBoolean [this] this)

(defrecord SLessThan [left right]
  Object
  (toString [_] (str left " < " right)))
(defmethod s-reducible? SLessThan [_] true)
(defmethod s-reduce SLessThan [{:keys [left right]}]
  (cond
    (s-reducible? left) (->SLessThan (s-reduce left) right)
    (s-reducible? right) (->SLessThan left (s-reduce right))
    :else (->SBoolean (< (.value left) (.value right)))))
