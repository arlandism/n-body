(ns n-bodies.core
  (:require [clojure.math.numeric-tower :refer [abs, expt]]))

(def GRAVITY 6.67384E-11)

(defn dimensional-difference [direction first-coordinate second-coordinate]
  (- (get first-coordinate direction 0)
     (get second-coordinate direction 0)))

(defn force-in-dimension-on-body [dimension target other-body]
  (let [distance (dimensional-difference dimension (target :position) (other-body :position))
        product-of-masses (reduce * (map :mass [target other-body]))
        distance-cubed (expt (abs distance) 3)]
    (if (zero? distance-cubed)
      0
      (/ (* distance product-of-masses GRAVITY) distance-cubed))))

(defn force-template [x y z]
  {:force_x x
   :force_y y
   :force_z z})

(defn force-on-one-body-from-another [body-one body-two]
   (force-template
     (force-in-dimension-on-body :x body-one body-two) 
     (force-in-dimension-on-body :y body-one body-two)
     (force-in-dimension-on-body :z body-one body-two)))

(defn sum-forces [force-one force-two]
  (force-template
    (reduce + (map :force_x [force-one  force-two])) 
    (reduce + (map :force_y [force-one  force-two])) 
    (reduce + (map :force_z [force-one force-two])) 
  ))

(defn sum-of-forces-on-one-body [body-one rest-of-bodies]
    (loop [forces-so-far {:force_x 0 :force_y 0 :force_z 0} other-bodies rest-of-bodies]
      (if (= 1 (count other-bodies))
        (sum-forces forces-so-far (force-on-one-body-from-another body-one (first other-bodies)))  
        (recur 
          (sum-forces 
            forces-so-far
            (force-on-one-body-from-another body-one (first rest-of-bodies)))
          (rest other-bodies)))))

(defn move-head-to-tail [coll]
  (drop 1 (conj coll (first coll))))
   
(defn compute-forces [bodies]
  (loop [forces-thus-far []  bodies bodies]
   (if (= (count forces-thus-far) (count bodies)) 
     forces-thus-far
     (recur
       (conj forces-thus-far (sum-of-forces-on-one-body (first bodies) (rest bodies)))
       (vec (move-head-to-tail bodies))))))
