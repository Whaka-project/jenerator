(ns jenerator.fns)

(defn int
  ([value] {:jenerate :int :value value})
  ([value base] {:jenerate :int :value value :base base}))

(defn long
  ([value] {:jenerate :int :value value :long true})
  ([value base] {:jenerate :int :value value :base base :long true}))

(defn- float*
  ([suffix whole]
    {:jenerate :float :whole whole :suffix suffix})
  ([suffix whole fraction {:keys [e shift] :or {e 0 shift 0}}]
    {:jenerate :float :whole whole :fraction fraction :exponent e :shift shift :suffix suffix}))

(defn float
  ([whole] (float* :f whole))
  ([whole fraction & {:keys [e shift] :or {e 0 shift 0} :as map}] (float* :f whole fraction map)))

(defn double
  ([whole] (float* :d whole))
  ([whole fraction & {:keys [e shift] :or {e 0 shift 0} :as map}] (float* :d whole fraction map)))
