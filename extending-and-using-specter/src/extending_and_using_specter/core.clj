#_{:clj-kondo/ignore [:refer-all]}
(ns extending-and-using-specter.core
  (:require [com.rpl.specter :as sp :refer :all])) ; nil

(comment
  (declare ALL collect-one))

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; Preface: Why Specter? ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;
;; Specter for selecting
;; ;;;;;;;;;;;;;;;;;;;;;

;; Clojure code
(let [x {:a [{:b 3}
             {:b 4}
             {:b 5 :c 3}]}]
  (get-in x [:a 1 :b])) ; vectors are associative structures with integer keys
; 4

;; Specter code
(let [x {:a [{:b 3}
             {:b 4}
             {:b 5 :c 3}]}]
  (sp/select [:a ALL :b] x))
; [3 4 5]

(let [x {:a [{:b 3}
             {:b 4}
             {:b 5 :c 3}]}]
  (sp/select [:a ALL :b odd?] x))
; [3 5]

(sp/select [(walker integer?) even?]
           '(+ 1 (- 2 3) (+ 4 (- 5 (+ 6 7)) 8)))
; [2 4 6 8]

;; ;;;;;;;;;;;;;;;;;;;;
;; Specter for updating
;; ;;;;;;;;;;;;;;;;;;;;

(sp/transform [(walker integer?) even?] ; selector
              (partial * 1000)          ; transformer
              '(+ 1 (- 2 3) (+ 4 (- 5 (+ 6 7)) 8)))
; (+ 1 (- 2000 3) (+ 4000 (- 5 (+ 6000 7)) 8000))

(let [input [{:a 1 :x {:y 2}}
             {:a 3 :x {:y 4}}]]
  (sp/transform [ALL :a] (partial * 1000) input))
; [{:a 1000, :x {:y 2}} {:a 3000, :x {:y 4}}]

(let [input [{:a 1 :x {:y 2}}
             {:a 3 :x {:y 4}}]]
  (sp/transform [ALL :x :y] (partial * 1000) input))
; [{:a 1, :x {:y 2000}} {:a 3, :x {:y 4000}}]

(let [input [{:a 1 :x {:y 2}}
             {:a 3 :x {:y 4}}]]
  (sp/select [ALL (collect-one :a) :x :y] input))
; [[1 2] [3 4]]

(let [input [{:a 1 :x {:y 2}}
             {:a 3 :x {:y 4}}]]
  (sp/transform [ALL (collect-one :a) :x :y]
                (fn [a y] (+ (* 1000 a) y))
                input))
; [{:a 1, :x {:y 1002}} {:a 3, :x {:y 3004}}]

;; ;;;;;;;;;;;;;;;;;;;;
;; Specter for learning
;; ;;;;;;;;;;;;;;;;;;;;
;; …

;; ;;;;;;;;;;;;;;;;;;;;;;;
;; How to Use this Book ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;
;; …

;; ;;;;;;;;;;;;;;;;;;;;;
;; Specter API exercises
;; ;;;;;;;;;;;;;;;;;;;;;

;; Specter's behavior with keywords

;; descends a map
(sp/select [:a]    {:a 1})      ; [1]
(sp/select [:a :b] {:a {:b 1}}) ; [1]

;; missing keyword has a value of `[nil]`
(sp/select [:a] {})         ; [nil]
(sp/select [:a] {:not-a 1}) ; [nil]
(sp/select [:a :b] {:a {}}) ; [nil]

;; as with `get`, bogus structures are silently accepted
;; it’s never possible for `get` of a keyword to fail; `select` has same behavior
(sp/select [:a] nil)       ; [nil]
(sp/select [:a] "no-map")  ; [nil]
(sp/select [:a :b] {:a 1}) ; [nil]

;; the result is specifically a vector
(sp/select [:a :b] {:a {:b 1}}) ; [1]

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specter implementation exercises
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; …

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Understanding Specter By Building It ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ALL, keyword, predicate
(sp/select [ALL :a even?]
           [{:a 1} {:a 2} {:a 3} {:a 4}])
; [2 4]

;; ;;;;;;;;
;; Keywords
;; ;;;;;;;;

;; see examples above

;; better solution would be with reduce
(defn select-kw [selectors structure]
  (let [[selector & rest-selectors] selectors]
    (if selector
      (select-kw rest-selectors (selector structure))
      (vector structure))))

(vec          [1 2 3]) ; [1 2 3]
(apply vector [1 2 3]) ; [1 2 3]

;; Behaves the way specter/select does
(select-kw [:a] nil)               ; [nil]
(select-kw [:a] :something-random) ; [nil]
(select-kw [:a] {:a 1})            ; [1]
(select-kw [:a] {:not-a 1})        ; [nil]
(select-kw [:a] {})                ; [nil]

(select-kw [:a :b] {:a {:b 1}})    ; [1]   ; the result is specifically a vector
(select-kw [:a :b] {:a 1})         ; [nil]
(select-kw [:a :b] {:a {}})        ; [nil]

;; ;;;;;;;;;;
;; Predicates
;; ;;;;;;;;;;

;; reference behavior for predicates

;; predicate success: the value is passed through to the result vector
(sp/select [odd?] 1) ; [1]

;; as is common, predicates don't have to be strictly true or false
(sp/select [#(get % :a)] {:a 1}) ; [{:a 1}]

;; predicate failure
(sp/select [even?] 1) ; []

;; (fact "given multiple predicates, each passes judgment in turn")
(sp/select [integer? odd?] 1)    ; [1]  ; the result is specifically a vector
(sp/select [integer? even?] 1)   ; []
(sp/select [integer? odd?] "hi") ; []

;; better solution would be with reduce
(defn select-pred [predicates candidate]
  (let [[predicate rest-predicates] predicates]
    (if predicate
      (if (predicate candidate)
        (select-pred (vector rest-predicates) candidate)
        [])
      [candidate])))

;; our implementation matches Specter's
(select-pred [odd?] 1)             ; [1]
(select-pred [even?] 1)            ; []
(select-pred [integer? odd?] 1)    ; [1]
(select-pred [integer? even?] 1)   ; []
(select-pred [integer? odd?] "hi") ; []

(defn select-both [[x & xs :as selector] structure]
  (cond
    (empty? selector) (vector structure)
    (keyword? x)      (select-both xs (get structure x))
    (fn? x)           (if (x structure)
                        (select-both xs structure)
                        [])))

(select-both [:a] nil)               ; [nil]
(select-both [:a] :something-random) ; [nil]
(select-both [:a] {:a 1})            ; [1]
(select-both [:a] {:not-a 1})        ; [nil]
(select-both [:a] {})                ; [nil]

(select-both [odd?] 1)             ; [1]
(select-both [even?] 1)            ; []
(select-both [integer? odd?] 1)    ; [1]
(select-both [integer? even?] 1)   ; []
(select-both [integer? odd?] "hi") ; []
