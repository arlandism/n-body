(ns n-bodies.core
  (:require [clojure.math.numeric-tower :refer [abs, expt, sqrt]]))

(def GRAVITY 6.67384E-11)

(defn move-head-to-tail [coll]
  (drop 1 (conj coll (first coll))))

(defn compute-f-on-dimensions [f first-coordinate second-coordinate]
  (into {} (for [dimension [:x :y :z]]
    [dimension (f (first-coordinate dimension) (second-coordinate dimension))]))) 

(defn scale-vector [scale vector]
  (into {} (for [[dimension value] vector] [dimension (* scale value)])))

(defn distance-formula [pos-one pos-two]
  (sqrt (reduce + (for [dimension (keys pos-one)]
    (expt (- (pos-one dimension) (pos-two dimension)) 2)))))

(defn calculate-constant [body-one body-two]
  (let [distance (distance-formula (body-one :position) (body-two :position))]
      (/
        (* GRAVITY (body-one :mass) (body-two :mass))
        (expt distance 3))))

(defn force-on-one-body-from-another [body-one body-two]
  (scale-vector 
    (calculate-constant body-one body-two) 
    (compute-f-on-dimensions - ( body-two :position) ( body-one :position)))) 

(defn sum-of-forces-on-one-body [body-one rest-of-bodies]
  (reduce (fn [forces-so-far current-body]
            (compute-f-on-dimensions +
              forces-so-far
              (force-on-one-body-from-another body-one current-body)))
          {:x 0 :y 0 :z 0}
          rest-of-bodies))
  
(defn compute-forces [bodies]
  (loop [forces-thus-far []  bodies bodies]
   (if (= (count forces-thus-far) (count bodies)) 
     forces-thus-far
     (recur
       (conj forces-thus-far (sum-of-forces-on-one-body (first bodies) (rest bodies)))
       (vec (move-head-to-tail bodies))))))
