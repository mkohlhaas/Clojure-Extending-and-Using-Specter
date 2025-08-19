#_{:clj-kondo/ignore [:refer-all]}
(ns extending-and-using-specter.core-test
  (:require [clojure.test :refer :all]
            [extending-and-using-specter.core :refer :all]))

(deftest a-test
  (testing "FIXED"
    (is (= 1 1))))
