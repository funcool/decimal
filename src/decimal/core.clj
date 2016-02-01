(ns decimal.core)

(defmacro with-config
  [config & body]
  `(let [type# (decimal.core/config ~config)]
     (binding [decimal.core/*decimal* type#]
       ~@body)))
