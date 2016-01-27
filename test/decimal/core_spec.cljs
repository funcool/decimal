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
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! *main-cli-fn* #(t/run-tests))

(defmethod t/report [:cljs.test/default :end-run-tests]
  [m]
  (if (t/successful? m)
    (set! (.-exitCode js/process) 0)
    (set! (.-exitCode js/process) 1)))
