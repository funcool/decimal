(ns decimal.core
  (:require [decimal.extern.decimaljs])
  (:refer-clojure :exclude [> >= < <= neg? pos? integer? zero? / - + *]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants & Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:static +decimal+ (js/Decimal.noConflict))
(def ^:dynamic *decimal* +decimal+)

(defn config!
  "Set the global configuration for the decimal constructor.

  Possible options:

  - `precision`: the maximum number of significant digits of
    the result of an operation (integer 1 to 1e+9 inclusive,
    default: 20).
  - `rounding`: the default rounding mode used when rounding
    the result of an operation (integer 0 to 8 inclusive,
  - `modulo`: he modulo mode used when calculating the modulus: `a mod n`.
    (integer, 0 to 9 inclusive, default 1

  **Rounding modes**

  Rounding modes 0 to 6 (inclusive) are the same as those of Java's
  BigDecimal class.

  Property         | Value | Description
  -----------------|-------|------------
  ROUND_UP         | 0     | Rounds away from zero
  ROUND_DOWN       | 1     | Rounds towards zero
  ROUND_CEIL       | 2     | Rounds towards Infinity
  ROUND_FLOOR      | 3     | Rounds towards -Infinity
  ROUND_HALF_UP    | 4     | Rounds towards nearest neighbour. If equidistant, rounds away from zero
  ROUND_HALF_DOWN  | 5     | Rounds towards nearest neighbour. If equidistant, rounds towards zero
  ROUND_HALF_EVEN  | 6     | Rounds towards nearest neighbour. If equidistant, rounds towards even neighbour
  ROUND_HALF_CEIL  | 7     | Rounds towards nearest neighbour. If equidistant, rounds towards Infinity
  ROUND_HALF_FLOOR | 8     | Rounds towards nearest neighbour. If equidistant, rounds towards -Infinity
  EUCLID           | 9     | Not a rounding mode, see modulo

  **Modulo modes**

  The modes that are most commonly used for the modulus/remainder operation
  are shown in the following table. Although the other rounding modes can be used,
  they may not give useful results.

  Property         | Value | Description
  -----------------|-------|------------
  ROUND_UP         | 0     | The remainder is positive if the dividend is negative, else is negative
  ROUND_DOWN       | 1     | The remainder has the same sign as the dividend. This uses truncating division and matches the behaviour of JavaScript's remainder operator %.
  ROUND_FLOOR      | 3     | The remainder has the same sign as the divisor. (This matches Python's % operator)
  ROUND_HALF_EVEN  | 6     | The IEEE 754 remainder function
  EUCLID           | 9     | The remainder is always positive.

  **Other options**

  The underlying library supports more options that and this
  function also accepts. You can read more about here:
  http://mikemcl.github.io/decimal.js/#Dconfig"
  [{:keys [precision rounding modulo] :as options}]
  (.config +decimal+ (clj->js options))
  nil)

(defn config
  "The same as `config` but returns an constructor
  of decimals that can be used for create new instances
  with provided configuration."
  [options]
  (.clone +decimal+ (clj->js options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols & Constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IDecimal
  (-decimal [v] "return a decimal instance."))

(defn decimal
  "Create a new Decimal instance from `v` value."
  [v]
  (-decimal v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn ^boolean decimal?
  "Return true if `v` is a instance of Decimal."
  [v]
  (instance? +decimal+ v))

(defn ^boolean finite?
  "Returns true if the value of this Decimal is a finite
  number, otherwise returns false.
  The only possible non-finite values of a Decimal are
  NaN, Infinity and -Infinity."
  [v]
  (.isFinite v))

(defn ^boolean integer?
  "Returns true if the value of this Decimal is a whole
  number, otherwise returns false."
  [v]
  (.isInteger v))

(defn ^boolean NaN?
  "Returns true if the value of this Decimal is NaN,
  otherwise returns false."
  [v]
  (.isNaN v))

(defn ^boolean neg?
  "Returns true if the value of this Decimal is negative,
  otherwise returns false."
  [v]
  (.isNegative v))

(defn ^boolean pos?
  "Returns true if the value of this Decimal is negative,
  otherwise returns false."
  [v]
  (.isPositive v))

(defn ^boolean zero?
  "Returns true if the value of this Decimal is zero or
  minus zero, otherwise returns false."
  [v]
  (.isZero v))

(defn ^boolean <
  "Returns true if the value of this Decimal is less than
  the value of x, otherwise returns false."
  ([v x]
   (.lt v x))
  ([v x & more]
   (if (< v x)
     (if (next more)
       (recur x (first more) (next more))
       (< x (first more)))
     false)))

(defn ^boolean <=
  "Returns true if the value of this Decimal is less than
  or equal to the value of x, otherwise returns false."
  ([v x]
   (.lte v x))
  ([v x & more]
   (if (<= v x)
     (if (next more)
       (recur x (first more) (next more))
       (<= x (first more)))
     false)))

(defn ^boolean >
  "Returns true if the value of this Decimal is greater than
  the value of x, otherwise returns false."
  ([v x]
   (.ge v x))
  ([v x & more]
   (if (> v x)
     (if (next more)
       (recur x (first more) (next more))
       (> x (first more)))
     false)))

(defn ^boolean >=
  "Returns true if the value of this Decimal is greater than
  or equal to the value of x, otherwise returns false."
  ([v x]
   (.ge v x))
  ([v x & more]
   (if (>= v x)
     (if (next more)
       (recur x (first more) (next more))
       (>= x (first more)))
     false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn plus
  "Returns a new Decimal whose value is the value of this
  Decimal plus x, rounded to significant digits."
  [v x]
  (.plus v x))

(defn minus
  "Returns a new Decimal whose value is the value of this
  Decimal minus x, rounded to significant digits."
  [v x]
  (.minus v x))

(defn mul
  "Returns a new Decimal whose value is the value of this
  Decimal times x, rounded to significant digits using."
  [v x]
  (.times v x))

(defn div
  "Returns a new Decimal whose value is the value of this
  Decimal divided by x, rounded to significant digits."
  [v x]
  (.div v x))

(defn div'
  "Return a new Decimal whose value is the integer part
  of dividing this Decimal by x, rounded to significant digits."
  [v x]
  (.divToInt v x))

;; Aliases

(def / div)
(def + plus)
(def - minus)
(def * mul)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn floor
  "Returns a new Decimal whose value is the value of this Decimal
  rounded to a whole number in the direction of negative Infinity."
  [v]
  (.floor v))

(defn ceil
  "Returns a new Decimal whose value is the value of this Decimal
  rounded to a whole number in the direction of positive Infinity."
  [v]
  (.ceil v))

(defn round
  "Returns a new Decimal whose value is the value of this Decimal
  rounded to a whole number.
  To emulate Math.round, set rounding to 7 (or :round/hanf-ceil)."
  [v]
  (.round v))

(defn abs
  "Returns a new Decimal whose value is the absolute value, i.e.
  the magnitude, of the value of this Decimal."
  [v]
  (.absoluteValue v))

(defn sin
  "Returns a new Decimal whose value is the sine of the value
  in radians of this Decimal."
  [v]
  (.sine v))

(defn cos
  "Returns a new Decimal whose value is the cosine of the value
  in radians of this Decimal."
  [v]
  (.cosine v))

(defn tan
  "Returns a new Decimal whose value is the tangent of the value
  in radians of this Decimal."
  [v]
  (.tangent v))

(defn cbrt
  "Returns a new Decimal whose value is the cube root of this Decimal."
  [v]
  (.cubeRoot v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type +decimal+
  cljs.core/IEquiv
  (-equiv [v other]
    (.eq v other))

  cljs.core/ICloneable
  (-clone [v]
    (let [t *decimal*]
      (t. v)))

  cljs.core/IPrintWithWriter
  (-pr-writer [mv writer _]
    (cljs.core/-write writer "#decimal ")
    (cljs.core/-write writer (.toString mv)))

  cljs.core/IDeref
  (-deref [v]
    (.toNumber v))

  IComparable
  (-compare [v other]
    (println "akakkaka")
    (.comparedTo v other)))

(extend-protocol IDecimal
  +decimal+
  (-decimal [v] v)

  string
  (-decimal [v]
    (*decimal*. v))

  number
  (-decimal [v]
    (*decimal*. v)))
