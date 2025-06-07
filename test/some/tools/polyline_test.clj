(ns some.tools.polyline-test
  (:require [some.tools.polyline :as pl]
            [clojure.test :as t]))

(t/deftest length
  (t/testing "pythagoras"
    (t/testing "2D"
      (t/is (= (pl/length [[0.0 0.0] [4.0 3.0]]) 5.0)))
    (t/testing "3D"
      (t/is (= (pl/length [[0.0 0.0 0.0] [4.0 3.0 0.0]]) 5.0)))
    (t/testing "3D-Z"
      (t/is (= (pl/length [[0.0 0.0 0.0] [0.0 3.0 4.0]]) 5.0))))
  (t/testing "duplicate points"
    (t/is (= (pl/length [[0.0 0.0][0.0 0.0][0.0 0.0][0.0 0.0]]) 0.0)))
  (t/testing "alternating repeat"
    (t/is (= (pl/length [[0.0 0.0]
                         [0.0 1.0]
                         [0.0 0.0]
                         [0.0 1.0]
                         [0.0 0.0]
                         [0.0 1.0]
                         [0.0 0.0]
                         [0.0 1.0]]) 7.0))))
