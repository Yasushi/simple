(ns simple.core)

(defmulti s-inspect "inspect expression" class)
(defmethod s-inspect :default [this] (str "«" (.toString this) "»"))

(defmulti s-reducible? "" class)
(defmethod s-reducible? :default [this] false)

(defmulti s-reduce ""
  (fn [& args] (class (first args))))

(defrecord SNumber [value]
  Object
  (toString [_] (str value)))
(defmethod s-reducible? SNumber [_] false)
(defmethod s-reduce SNumber [this _] this)

(defrecord SAdd [left right]
  Object
  (toString [_] (str (.toString left) " + " (.toString right))))
(defmethod s-reducible? SAdd [_] true)
(defmethod s-reduce SAdd [{:keys [left right]} env]
  (cond
    (s-reducible? left) (->SAdd (s-reduce left env) right)
    (s-reducible? right) (->SAdd left (s-reduce right env))
    :else (->SNumber (+ (.value left) (.value right)))))

(defrecord SMultiply [left right]
  Object
  (toString [_] (str (.toString left) " * " (.toString right))))
(defmethod s-reducible? SMultiply [_] true)
(defmethod s-reduce SMultiply [{:keys [left right]} env]
  (cond
    (s-reducible? left) (->SMultiply (s-reduce left env) right)
    (s-reducible? right) (->SMultiply left (s-reduce right env))
    :else (->SNumber (* (.value left) (.value right)))))

(defn s-run [expression env]
  (cons expression
        (if (s-reducible? expression)
          (lazy-seq (s-run (s-reduce expression env) env)))))

(defrecord SBoolean [value]
  Object
  (toString [_] (str value)))
(defmethod s-reducible? SBoolean [_] false)
(defmethod s-reduce SBoolean [this _] this)

(defrecord SLessThan [left right]
  Object
  (toString [_] (str left " < " right)))
(defmethod s-reducible? SLessThan [_] true)
(defmethod s-reduce SLessThan [{:keys [left right]} env]
  (cond
    (s-reducible? left) (->SLessThan (s-reduce left env) right)
    (s-reducible? right) (->SLessThan left (s-reduce right env))
    :else (->SBoolean (< (.value left) (.value right)))))

(defrecord SVariable [name]
  Object
  (toString [_] (str name)))
(defmethod s-reducible? SVariable [_] true)
(defmethod s-reduce SVariable [this env]
  (get env (.name this)))
