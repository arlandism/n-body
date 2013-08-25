(ns n-bodies.core
  (:require [clojure.math.numeric-tower :refer [abs, expt, sqrt]]))

(def GRAVITY 6.67384E-11)

(defn move-head-to-tail [coll]
  (drop 1 (conj coll (first coll))))

(defn scale-vector [scale v]
  {:x (* scale (v :x))
   :y (* scale (v :y))
   :z (* scale (v :z))})

(defn force-template [x y z]
  {:x x
   :y y
   :z z})

(defn distance-formula [pos-one pos-two]
  (let [a-squared (expt (- (pos-one :x) (pos-two :x)) 2)
        b-squared (expt (- (pos-one :y) (pos-two :y)) 2)
        c-squared (expt (- (pos-one :z) (pos-two :z)) 2)]
  (sqrt
    (+ a-squared b-squared c-squared))))

(defn calculate-constant [body-one body-two]
  (let [distance (distance-formula (body-one :position) (body-two :position))]
    (if (zero? distance) 
      0
      (/ 
        (* GRAVITY (body-one :mass) (body-two :mass))
        (expt (abs distance) 3)))))

 (defn dimensional-difference [direction first-coordinate second-coordinate]
  (reduce - (map direction [first-coordinate second-coordinate])))

(defn vector-difference [first-coordinate second-coordinate]
  (force-template
    (- (first-coordinate :x) (second-coordinate :x))
    (- (first-coordinate :y) (second-coordinate :y))
    (- (first-coordinate :z) (second-coordinate :z))))

(defn force-in-dimension-on-body [dimension target-body other-body]
  (let [distance (dimensional-difference dimension (target-body :position) (other-body :position))
        product-of-masses (reduce * (map :mass [target-body other-body]))
        distance-cubed (expt (abs distance) 3)]
    (if (zero? distance-cubed)
      0
      (/ (* distance product-of-masses GRAVITY) distance-cubed))))

(defn force-on-one-body-from-another [body-one body-two]
  (scale-vector (calculate-constant body-one body-two) (vector-difference (body-one :position) (body-two :position))))
   ;(force-template
   ;  (force-in-dimension-on-body :x body-one body-two) 
   ;  (force-in-dimension-on-body :y body-one body-two)
   ;  (force-in-dimension-on-body :z body-one body-two)))

(defn sum-forces [force-one force-two]
  (force-template
    (reduce + (map :x [force-one  force-two])) 
    (reduce + (map :y [force-one  force-two])) 
    (reduce + (map :z [force-one force-two]))))

(defn sum-of-forces-on-one-body [body-one rest-of-bodies]
  (reduce (fn [forces-so-far current-body]
            (sum-forces
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
