(ns some.tools.spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::normalized-number (s/and number? #(<= 0.0 %) #(<= % 1.0)))

(s/def ::non-infinite-number (s/and double? #(-> % infinite? not) #(-> % NaN? not)))

(s/def ::point (s/coll-of double? :kind vector? :min-count 2 :max-count 3))

(s/def ::non-infinite-point (s/coll-of ::non-infinite-number
                                       :kind vector?
                                       :min-count 2
                                       :max-count 3))

(s/def ::polyline
  (s/or :2d (s/coll-of (s/coll-of double? :kind vector? :count 2)
                       :kind vector?
                       :min-count 2)
        :3d (s/coll-of (s/coll-of double? :kind vector? :count 3)
                       :kind vector?
                       :min-count 2)))

;;; some.tools.scalar

(s/fdef some.tools.scalar/mix
  :args (s/and (s/cat :a ::non-infinite-number
                      :b ::non-infinite-number
                      :t ::normalized-number))
  :ret double?
  :fn #(let [a (-> % :args :a)
             b (-> % :args :b)
             ret (-> % :ret)]
         (if (< a b)
           (and (<= a ret) (<= ret b))
           (and (<= b ret) (<= ret a)))))

;;; some.tools.vec

(s/fdef some.tools.vec/+
  :args (s/and (s/cat :v1 ::point :v2 ::point)
               #(= (count (:v1 %)) (count (:v2 %))))
  :ret ::point
  :fn #(= (count (:ret %)) (count (-> % :args :v1))))

(s/fdef some.tools.vec/-
  :args (s/and (s/cat :v1 ::point :v2 ::point)
               #(= (count (:v1 %)) (count (:v2 %))))
  :ret ::point
  :fn #(= (count (:ret %)) (count (-> % :args :v1))))

(s/fdef some.tools.vec/*
  :args (s/and (s/cat :v1 ::point :v2 ::point)
               #(= (count (:v1 %)) (count (:v2 %))))
  :ret ::point
  :fn #(= (count (:ret %)) (count (-> % :args :v1))))

(s/fdef some.tools.vec//
  :args (s/and (s/cat :v1 ::point :v2 ::point)
               #(not-any? zero? (:v2 %))
               #(= (count (:v1 %)) (count (:v2 %))))
  :ret ::point
  :fn #(= (count (:ret %)) (count (-> % :args :v1))))

(s/fdef some.tools.vec/length
  :args (s/cat :v ::point)
  :ret (s/or :valid-length (s/and double? pos?)
             :invalid-length NaN?
             :no-length zero?))

(s/fdef some.tools.vec/distance
  :args (s/and (s/cat :v1 ::point :v2 ::point)
               #(= (count (:v1 %)) (count (:v2 %))))
  :ret (s/or :valid-length (s/and double? pos?)
             :invalid-length NaN?
             :no-length zero?))

(s/fdef some.tools.vec/mix
  :args (s/and (s/cat :v1 ::non-infinite-point
                      :v2 ::non-infinite-point
                      :t ::normalized-number)
               #(= (count (:v1 %)) (count (:v2 %))))
  :ret ::point)

(s/def :dx->x/start double?)

(s/fdef some.tools.vec/dx->x
  :args (s/cat :dx (s/coll-of number? :kind vector? :min-count 1)
               :kwargs (s/keys :opt [:dx->x/start]))
  :ret (s/coll-of number? :kind vector? :min-count 2)
  :fn #(and (= (-> % :args :dx count inc) (-> % :ret count))
            (or (-> % :ret first zero?)
                (= (-> % :ret first) (-> % :args :kwargs :start)))))

;;; some.tools.polyline

(s/fdef some.tools.polyline/length
  :args (s/cat :polyline ::polyline)
  :ret double?)

(s/fdef some.tools.polyline/index-at-length
  :args (s/cat :polyline ::polyline :length double?)
  :ret (s/or :within-bounds double? :out-of-bounds nil?)
  :fn #(let [l (-> % :args :length)
             [path i] (:ret %)]
         (cond (< l 0.0) (nil? i)
               (zero? l) (zero? i)
               :else (or (double? i) (nil? i)))))
