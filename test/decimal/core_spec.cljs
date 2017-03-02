(ns decimal.core-spec
  (:require [cljs.test :as t]
            [decimal.core :as dc]))

(enable-console-print!)

(t/deftest constructor-tests
  (let [d1 (dc/decimal "1")
        d2 (dc/decimal "1.2")
        d3 (dc/decimal "-0.4e-4")
        d4 (dc/decimal -5.3)]
    (t/is (dc/decimal? d1))
    (t/is (dc/integer? d1))
    (t/is (dc/pos? d1))
    (t/is (not (dc/neg? d2)))
    (t/is (= d1 1))
    (t/is (= @d1 1))

    (t/is (dc/decimal? d2))
    (t/is (not (dc/integer? d2)))
    (t/is (dc/pos? d2))
    (t/is (not (dc/neg? d2)))
    (t/is (= d2 1.2))
    (t/is (= @d2 1.2))

    (t/is (dc/decimal? d3))
    (t/is (not (dc/integer? d3)))
    (t/is (not (dc/pos? d3)))
    (t/is (dc/neg? d3))))

(t/deftest compare-tests
  (let [d1 (dc/decimal "1.2")
        d2 (dc/decimal "5.0")
        d3 (dc/decimal "10.99999")]
    (t/is (> d2 d1))
    (t/is (< d1 d3))
    (t/is (= d3 "10.99999"))
    (t/is (>= d3 "10.99999"))
    (t/is (<= d3 "10.99999"))
    (t/is (dc/> d2 d1))
    (t/is (dc/< d1 d3))
    (t/is (dc/>= d3 "10.99999"))
    (t/is (dc/<= d3 "10.99999"))
    ))

(def ^:static +test-values+
  ["0", "-0", "-0.5", "-0.53", "-0.409325859"
   "0.1", "0.0799", "0.0769104749433424068"])

(defn equal-or-both-NaN
  [value1 value2]
  (t/is (or
    (and (dc/NaN? value1) (dc/NaN? value2))
    (= value1 value2))))

(t/deftest api-test
  (doseq [v +test-values+]
    (equal-or-both-NaN (dc/abs v) (.abs dc/+decimal+ v))
    (t/is (= (dc/finite? v) (.isFinite (dc/-decimal v))))
    (t/is (= (dc/integer? v) (.isInteger (dc/-decimal v))))
    (t/is (= (dc/NaN? v) (.isNaN (dc/-decimal v))))
    (t/is (= (dc/neg? v) (.isNegative (dc/-decimal v))))
    (t/is (= (dc/pos? v) (.isPositive (dc/-decimal v))))
    (t/is (= (dc/zero? v) (.isZero (dc/-decimal v))))
    (t/is (= (dc/< v 0) (.lt (dc/-decimal v) 0)))
    (t/is (= (dc/<= v 0) (.lte (dc/-decimal v) 0)))
    (t/is (= (dc/> v 0) (.gt (dc/-decimal v) 0)))
    (t/is (= (dc/>= v 0) (.gte (dc/-decimal v) 0)))
    (t/is (= (dc/= v 0) (.eq (dc/-decimal v) 0)))
    (equal-or-both-NaN (dc/plus v 1) (.plus (dc/-decimal v) 1))
    (equal-or-both-NaN (dc/minus v 1) (.minus (dc/-decimal v) 1))
    (equal-or-both-NaN (dc/mul v 2) (.times (dc/-decimal v) 2))
    (equal-or-both-NaN (dc/div v 2) (.div (dc/-decimal v) 2))
    (equal-or-both-NaN (dc/floor v) (.floor (dc/-decimal v)))
    (equal-or-both-NaN (dc/round v) (.round (dc/-decimal v)))
    (equal-or-both-NaN (dc/ceil v) (.ceil (dc/-decimal v)))
    (equal-or-both-NaN (dc/sin v) (.sin (dc/-decimal v)))
    (equal-or-both-NaN (dc/cos v) (.cos (dc/-decimal v)))
    (equal-or-both-NaN (dc/tan v) (.tan (dc/-decimal v)))
    (equal-or-both-NaN (dc/asin v) (.asin (dc/-decimal v)))
    (equal-or-both-NaN (dc/acos v) (.acos (dc/-decimal v)))
    (equal-or-both-NaN (dc/atan v) (.atan (dc/-decimal v)))
    (equal-or-both-NaN (dc/sinh v) (.sinh (dc/-decimal v)))
    (equal-or-both-NaN (dc/cosh v) (.cosh (dc/-decimal v)))
    (equal-or-both-NaN (dc/tanh v) (.tanh (dc/-decimal v)))
    (equal-or-both-NaN (dc/asinh v) (.asinh (dc/-decimal v)))
    (equal-or-both-NaN (dc/acosh v) (.acosh (dc/-decimal v)))
    (equal-or-both-NaN (dc/atanh v) (.atanh (dc/-decimal v)))
    (equal-or-both-NaN (dc/cbrt v) (.cubeRoot (dc/-decimal v)))
    (equal-or-both-NaN (dc/sqrt v) (.squareRoot (dc/-decimal v)))
    (equal-or-both-NaN (dc/log v 2) (.log (dc/-decimal v) 2))
    (equal-or-both-NaN (dc/log2 v) (.log2 dc/+decimal+ v))
    (equal-or-both-NaN (dc/log10 v) (.log10 dc/+decimal+ v))
    (equal-or-both-NaN (dc/mod v 2) (.mod (dc/-decimal v) 2))
    (equal-or-both-NaN (dc/cmp v 0) (.cmp (dc/-decimal v) 0))
    ))

;; (t/deftest operations-test
;;   (t/testing "abs"
;;     (t/is (= (dc/abs 0) "0"))
;;     (t/is (= (dc/abs -0) "0"))
;;     (t/is (= (dc/abs "-0.5") "0.5")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! *main-cli-fn* #(t/run-tests))

(defmethod t/report [:cljs.test/default :end-run-tests]
  [m]
  (if (t/successful? m)
    (set! (.-exitCode js/process) 0)
    (set! (.-exitCode js/process) 1)))
