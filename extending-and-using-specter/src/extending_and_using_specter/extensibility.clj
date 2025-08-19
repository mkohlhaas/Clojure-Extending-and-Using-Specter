(ns extending-and-using-specter.extensibility)

;; ;;;;;;;;;;;;;
;; Extensibility
;; ;;;;;;;;;;;;;

(defprotocol Navigator
  (select* [this remainder structure]))

(defn select [[x & xs :as selector] structure]
  (if (empty? selector)
    (vector structure)
    (select* x xs structure)))

(extend-type clojure.lang.Keyword
  Navigator
  (select* [this remainder structure]
    (select remainder (get structure this))))

(extend-type clojure.lang.AFn
  Navigator
  (select* [this remainder structure]
    (if (this structure)
      (select remainder structure)
      [])))

(select [:a :b] {:a {:b 1}}) ; [1]

;; ;;;
;; ALL
;; ;;;

(deftype AllType [])

(def ALL (->AllType)) ; `ALL` is the only instance of `AllType`

(extend-type AllType)
2   Navigator
3   (select* [this structure continuation])
4     (into [] (mapcat continuation structure))

