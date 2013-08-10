(ns n-bodies.core
  (:require [clojure.math.numeric-tower :refer [abs, expt]]))

(declare vector-difference)

(def GRAVITY 6.67384E-11)

(defn dimensional-difference [direction first-coordinate second-coordinate]
  (- (get first-coordinate direction 0)
     (get second-coordinate direction 0)))

(defn force-in-dimension-on-body [dimension target other-body]
  (let [distance (dimensional-difference dimension (target :position) (other-body :position))
        product-of-masses (* (target :mass) (other-body :mass))
        positive-distance-cubed (expt (abs distance) 3)]
    (if (zero? positive-distance-cubed)
      0
      (/ (* product-of-masses distance GRAVITY) positive-distance-cubed))))

(defn force-on-one-body-from-another [body-one body-two]
    (loop [dimension-axis-pairs [[:force_x :x] [:force_y :y] [:force_z :z]]
           direction-value {}]
      (if (empty? dimension-axis-pairs) 
        direction-value 
        (let [[dimension axis] (first dimension-axis-pairs) 
              force-val (force-in-dimension-on-body axis body-one body-two)]
        (recur 
          (rest dimension-axis-pairs) (assoc direction-value dimension force-val))))))
   ; Is this better????
   ; {:force_x (force-in-dimension-on-body :x body-one body-two), 
   ; :force_y (force-in-dimension-on-body :y body-one body-two),
   ; :force_z (force-in-dimension-on-body :z body-one body-two)})

(defn sum-forces [force-one force-two]
  {
   :force_x (+ force-one :x force-two :x) 
   :force_y (+ force-one :y force-two :y) 
   :force_z (+ force-one :z force-two :z) 
   })

(defn sum-of-forces-on-one-body [body-one rest-of-bodies]
  (force-on-one-body-from-another body-one (first rest-of-bodies)))
  
  ;{:force_x(+ 
  ;    ((force-on-one-body-from-another body-one (first rest-of-bodies)) :x) 
  ;    ((force-on-one-body-from-another body-one (second rest-of-bodies)) :x)
  ; :force_y(+ 
  ;    ((force-on-one-body-from-another body-one (first rest-of-bodies)) :y)  
  ;    ((force-on-one-body-from-another body-one (second rest-of-bodies)) :y))
  ; :force_z(+ 
  ;    ((force-on-one-body-from-another body-one (first rest-of-bodies)) :z)  
  ;    ((force-on-one-body-from-another body-one (second rest-of-bodies)) :z)))})

(defn force-calculator [bodies]
 [(force-on-one-body-from-another (first bodies) (second bodies)) 
  (force-on-one-body-from-another (second bodies) (first bodies))])
