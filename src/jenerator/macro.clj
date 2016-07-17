(ns jenerator.macro)

(defmacro int
  ([value] `{:jenerate :int :value ~value})
  ([value base] `{:jenerate :int :value ~value :base ~base}))

(defmacro long
  ([value] `{:jenerate :int :value ~value :long true})
  ([value base] `{:jenerate :int :value ~value :base ~base :long true}))

(defmacro float
  ([whole]
    `{:jenerate :float :whole ~whole})
  ([whole fraction & {:keys [e shift] :or {shift 0 exponent 0}}]
    `{:jenerate :float :whole ~whole :fraction ~fraction :exponent ~e :shift ~shift}))