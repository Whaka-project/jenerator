(ns jenerator.fns
  (:refer-clojure :exclude [int long float double type]))

(defn int
  ([value] {:jtag :int :value value})
  ([value base] {:jtag :int :value value :base base}))

(defn long
  ([value] {:jtag :int :value value :long true})
  ([value base] {:jtag :int :value value :base base :long true}))

(defn- float*
  ([suffix whole]
    {:jtag :float :whole whole :suffix suffix})
  ([suffix whole fraction {:keys [e shift] :or {e 0 shift 0}}]
    {:jtag :float :whole whole :fraction fraction :exponent e :shift shift :suffix suffix}))

(defn float
  ([whole] (float* :f whole))
  ([whole fraction & {:keys [e shift] :or {e 0 shift 0} :as map}] (float* :f whole fraction map)))

(defn double
  ([whole] (float* :d whole))
  ([whole fraction & {:keys [e shift] :or {e 0 shift 0} :as map}] (float* :d whole fraction map)))

(defn ann
  ([class] (ann class :args {}))
  ([class value] (ann class :value value))
  ([class arg-key arg-value]
    (let [map (if (and (= :args arg-key) (map? arg-value)) arg-value {arg-key arg-value})]
      {:jtag :annotation :class class :args map}))
  ([class arg-key arg-value & {:as other-args}]
    (ann class :args (assoc other-args arg-key arg-value))))

(defn type
  ([types] (type types 0))
  ([types array]
    (let [ftype (if (sequential? types) (first types) types)
          generics (if (sequential? types) 
                     (map #(if (or (class? %) (sequential? %)) (type %) %) (rest types)) nil)]
      {:jtag :type :type ftype :generics generics :array array})))
