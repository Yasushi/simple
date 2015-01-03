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

(defn s-run [statement env]
  (cons [statement env]
        (if (s-reducible? statement)
          (lazy-seq
           (let [run
                 (fn
                   ([st ev] (s-run st ev))
                   ([exp] (s-run exp env)))]
             (apply run (s-reduce statement env)))))))

(defn s-run-inspect [statement env]
  (map (fn [[s e]]
         [(s-inspect s) (into {} (for [[k v] e] [k (s-inspect v)]))])
       (s-run statement env)))

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

(defrecord SDoNothing []
  Object
  (toString [_] "do-nothing"))
(defmethod s-reducible? SDoNothing [_] false)
(defmethod s-reduce SDoNothing [this _] this)

(defrecord SAssign [name expression]
  Object
  (toString [_] (str name " = " expression)))
(defmethod s-reducible? SAssign [_] true)
(defmethod s-reduce SAssign [{:keys [name expression]} env]
  (if (s-reducible? expression)
    [(->SAssign name (s-reduce expression env)) env]
    [(->SDoNothing) (merge env {name expression})]))

(defrecord SIf [condition consequence alternative]
  Object
  (toString [_] (str "if ( " condition " ) then { " consequence " } else { " alternative " }")))
(defmethod s-reducible? SIf [_] true)
(defmethod s-reduce SIf [{:keys [condition consequence alternative]} env]
  (if (s-reducible? condition)
    [(->SIf (s-reduce condition env) consequence alternative) env]
    (cond
      (= condition (->SBoolean true)) [consequence env]
      (= condition (->SBoolean false)) [alternative env])))

(defrecord SSequence [first second]
  Object
  (toString [_] (str first "; " second)))
(defmethod s-reducible? SSequence [_] true)
(defmethod s-reduce SSequence [{:keys [first second]} env]
  (if (= first (->SDoNothing))
    [second env]
    (let [[rfirst renv] (s-reduce first env)]
      [(->SSequence rfirst second) renv])))
