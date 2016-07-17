(ns jenerator.macro)

(defmacro int
  ([value] `(jenerator.macro/int ~value :dec false))
  ([value base] `(jenerator.macro/int ~value ~base false))
  ([value base long]
    `{:jenerate :int
      :value ~value
      :base ~base
      :long ~long}))

(defmacro float
  ([whole] `(jenerator.macro/float ~whole 0 nil))
  ([whole fraction] `(jenerator.macro/float ~whole ~fraction nil))
  ([whole fraction exponent]
    `{:jenerate :float
      :whole ~whole
      :fraction ~fraction
      :exponent ~exponent}))