(ns decimal.core
  (:require [decimal.extern.decimaljs])
  (:refer-clojure :exclude [> >= < <= neg? pos? integer? zero? / - + * max min mod]))

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
  (.isFinite (-decimal v)))

(defn ^boolean integer?
  "Returns true if the value of this Decimal is a whole
  number, otherwise returns false."
  [v]
  (.isInt (-decimal v)))

(defn ^boolean NaN?
  "Returns true if the value of this Decimal is NaN,
  otherwise returns false."
  [v]
  (.isNaN (-decimal v)))

(defn ^boolean neg?
  "Returns true if the value of this Decimal is negative,
  otherwise returns false."
  [v]
  (.isNegative (-decimal v)))

(defn ^boolean pos?
  "Returns true if the value of this Decimal is negative,
  otherwise returns false."
  [v]
  (.isPositive (-decimal v)))

(defn ^boolean zero?
  "Returns true if the value of this Decimal is zero or
  minus zero, otherwise returns false."
  [v]
  (.isZero (-decimal v)))

(defn ^boolean <
  "Returns true if the value of this Decimal is less than
  the value of x, otherwise returns false."
  ([v x]
   (.lt (-decimal v) x))
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
   (.lte (-decimal v) x))
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
   (.gt (-decimal v) x))
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
   (.gte (-decimal v) x))
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
  (.plus (-decimal v) x))

(defn minus
  "Returns a new Decimal whose value is the value of this
  Decimal minus x, rounded to significant digits."
  [v x]
  (.minus (-decimal v) x))

(defn mul
  "Returns a new Decimal whose value is the value of this
  Decimal times x, rounded to significant digits using."
  [v x]
  (.times (-decimal v) x))

(defn div
  "Returns a new Decimal whose value is the value of this
  Decimal divided by x, rounded to significant digits."
  [v x]
  (.div (-decimal v) x))

(defn div'
  "Return a new Decimal whose value is the integer part
  of dividing this Decimal by x, rounded to significant digits."
  [v x]
  (.divToInt (-decimal v) x))

(defn max
  "Returns a new Decimal whose value is the maximum."
  [v & args]
  (apply Decimal.max args))

(defn min
  "Returns a new Decimal whose value is the minimum."
  [v & args]
  (apply Decimal.min args))

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
  (.floor (-decimal v)))

(defn ceil
  "Returns a new Decimal whose value is the value of this Decimal
  rounded to a whole number in the direction of positive Infinity."
  [v]
  (.ceil (-decimal v)))

(defn round
  "Returns a new Decimal whose value is the value of this Decimal
  rounded to a whole number.
  To emulate Math.round, set rounding to 7 (or :round/hanf-ceil)."
  [v]
  (.round (-decimal v)))

(defn abs
  "Returns a new Decimal whose value is the absolute value, i.e.
  the magnitude, of the value of this Decimal."
  [v]
  (.absoluteValue (-decimal v)))

(defn sin
  "Returns a new Decimal whose value is the sine of the value
  in radians of this Decimal."
  [v]
  (.sine (-decimal v)))

(defn asin
  "Returns a new Decimal whose value is the inverse sine in
  radians of the value of this Decimal."
  [v]
  (.inverseSine (-decimal v)))

(defn sinh
  "Returns a new Decimal whose value is the hyperbolic sine of
  the value in radians of this Decimal."
  [v]
  (.hyperbolicSine (-decimal v)))

(defn asinh
  "Returns a new Decimal whose value is the inverse hyperbolic
  sine in radians of the value of this Decimal."
  [v]
  (.inverseHyperbolicSine (-decimal v)))

(defn cos
  "Returns a new Decimal whose value is the cosine of the value
  in radians of this Decimal."
  [v]
  (.cosine (-decimal v)))

(defn acos
  "Returns a new Decimal whose value is the inverse cosine
  in radians of the value of this Decimal."
  [v]
  (.inverseCosine (-decimal v)))

(defn cosh
  "Returns a new Decimal whose value is the hyperbolic cosine
  of the value in radians of this Decimal."
  [v]
  (.hyperbolicCosine (-decimal v)))

(defn acosh
  "Returns a new Decimal whose value is the inverse hyperbolic
  cosine in radians of the value of this Decimal."
  [v]
  (.inverseHyperbolicCosine (-decimal v)))

(defn tan
  "Returns a new Decimal whose value is the tangent of the value
  in radians of this Decimal."
  [v]
  (.tangent (-decimal v)))

(defn atan
  "Returns a new Decimal whose value is the inverse tangent
  in radians of the value of this Decimal."
  [v]
  (.inverseTangent (-decimal v)))

(defn tanh
  "Returns a new Decimal whose value is the hyperbolic
  tangent of the value in radians of this Decimal."
  [v]
  (.hyperbolicTangent (-decimal v)))

(defn atanh
  "Returns a new Decimal whose value is the inverse hyperbolic
  tangent in radians of the value of this Decimal."
  [v]
  (.inverseHyperbolicTangent (-decimal v)))

(defn cbrt
  "Returns a new Decimal whose value is the cube root of this Decimal."
  [v]
  (.cubeRoot (-decimal v)))

(defn sqrt
  "Returns a new Decimal whose value is the square root of this Decimal."
  [v]
  (.squareRoot (-decimal v)))

(defn log
  "Returns a new Decimal whose value is the base `x` logarithm
  of the value of this Decimal.
  If x is omitted, the base 10 logarithm of the value of
  this Decimal will be returned."
  ([v]
   (log v 10))
  ([v x]
   (.log (-decimal v) x)))

(defn log2
  "Returns a new Decimal whose value is the base 2 logarithm of `x`."
  [x]
  (.log2 *decimal* x))

(defn log10
  "Returns a new Decimal whose value is the base 10 logarithm of `x`."
  [x]
  (.log10 *decimal* x))


(defn mod
  "Returns a new Decimal whose value is the value of
  this Decimal modulo `x`.

  The value returned, and in particular its sign, is
  dependent on the value of the modulo property of this
  Decimal's constructor. If it is 1 (default value), the
  result will have the same sign as this Decimal, and it
  will match that of Javascript's `%` operator (within
  the limits of double precision) and `BigDecimal`'s
  remainder method.

  See `config!` function docstrings for a description
  and available options for `modulo`."
  [v x]
  (.mod (-decimal v) x))

(defn exp
  "Returns a new Decimal whose value is the base e (Euler's number,
  the base of the natural logarithm) exponential of the value of
  this Decimal.

  The `ln` is the invese of this function."
  [v]
  (.naturalExponential (-decimal v)))

(defn ln
  "Returns a new Decimal whose value is the natural logarithm of
  the value of this Decimal."
  [v]
  (.naturalLogarithm (-decimal v)))

(defn hypot
  "Returns a new Decimal whose value is the square root of the sum
  of the squares of the arguments."
  [& params]
  (apply Decimal.hypot params))

(defn neg
  "Returns a new Decimal whose value is the value of this Decimal negated."
  [v]
  (.negated (-decimal v)))

(defn random
  "Returns a new Decimal with a pseudo-random value equal to or greater
  than 0 and less than 1.
  The return value will have `dp` decimal places (or less if trailing
  zeros are produced). If dp is omitted then the default will be used.

  Detailed doc: http://mikemcl.github.io/decimal.js/#Drandom"
  [dp]
  (.random *decimal* dp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introspection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn decimal-paces
  "Returns the number of decimal places, i.e. the number
  of digits after the decimal point, of the value of this Decimal."
  [v]
  (.decimalPlaces (-decimal v)))

(defn truncate
  "Returns a new Decimal whose value is the value of this Decimal
  truncated to a whole number."
  [v]
  (.truncate (-decimal v)))

(defn precision
  "Returns the number of significant digits of the value of this Decimal.

  If `include-zeros` is `true`, then the trailing zeros of the integer
  part will be included in the counter of significant digits."
  ([v]
   (precision v false))
  ([v include-zeros]
   (.precision (-decimal v) (boolean include-zeros))))

(defn sign
  "Returns the sign of the value of this Decimal."
  [v]
  (.sign *decimal* v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn to-binary
  "Returns a string representing the value of this Decimal in binary format.

  If `sd` is specified, the return value will use binary exponential notation.
  If `sd` is omitted, the return value will be rounded to default significant
  digits. If `rm` is omitted, default rounding mode will be used."
  ([v] (to-binary v nil nil))
  ([v sd] (to-binary v sd nil))
  ([v sd rm]
   (let [v (-decimal v)]
     (.toBinary v sd rm))))

(defn to-exponential
  "Returns a string representing the value of this Decimal in
  exponential notation rounded using rounding mode `rm` to `dp`
  decimal places.

  If the value of this Decimal in exponential notation has fewer
  than `dp` fraction digits, the return value will be appended
  with zeros accordingly.

  If `dp` is omitted, the number of digits after the decimal
  point defaults to the minimum number of digits necessary to
  represent the value exactly.

  If `rm` is omitted, rounding mode rounding is used.

  Throws on an invalid `dp` or `rm` value."
  ([v] (to-exponential v nil nil))
  ([v dp] (to-exponential v dp nil))
  ([v dp rm]
   (.toExponential (-decimal v) dp rm)))

(defn to-fixed
  "Returns a string representing the value of this Decimal in
  normal (fixed-point) notation rounded to `dp` decimal places
  using rounding mode `rm`.

  If the value of this Decimal in normal notation has fewer
  than `dp` fraction digits, the return value will be appended
  with zeros accordingly.

  Unlike `Number/toFixed`, which returns exponential notation
  if a number is greater or equal to 10^21, this method will
  always return normal notation.

  If `dp` is omitted, the return value will be unrounded and
  in normal notation. This is unlike `Number/toFixed`, which
  returns the value to zero decimal places, but is useful when
  because of the current `toExpNeg` or `toExpNeg` values,
  toString returns exponential notation.

  If `rm` is omitted, default rounding mode is used.

  Throws on an invalid `dp` or `rm` value."
  ([v] (to-fixed v nil nil))
  ([v dp] (to-fixed v dp nil))
  ([v dp rm]
   (.toFixed (-decimal v) dp rm)))

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
