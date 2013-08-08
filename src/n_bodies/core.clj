(ns n-bodies.core
  (:require [clojure.math.numeric-tower :refer [abs, expt, sqrt]]))

(declare vector-difference)

(def GRAVITY 6.67384E-11)

(defn dimensional-difference [direction first-coordinate second-coordinate]
  (- (get first-coordinate direction 0)
     (get second-coordinate direction 0))
  )

(defn square [x]
  (* x x))

(defn force-in-dimension-on-body [dimension target other-body]
  (let [distance (dimensional-difference dimension (target :position) (other-body :position))
        product-of-masses (* (target :mass) (other-body :mass))
        positive-distance-cubed (expt (abs distance) 3)]
    (/ (* product-of-masses distance GRAVITY) positive-distance-cubed)))

(defn force-on-one-body-from-another [body-one body-two]
  (let [force-dimensions [[:force_x :x] [:force_y :y] [:force_z :z]]]
    (loop [force-dimensions force-dimensions dictionary {}]
      (if (empty? force-dimensions) 
        dictionary 
        (let [[force-direction axis] (first force-dimensions)]
        (recur 
          (rest force-dimensions) (assoc dictionary force-direction (force-in-dimension-on-body axis body-one body-two))))))))

   ; Is this better????
   ;{:force_x (force-in-dimension-on-body :x body-one body-two), 
   ; :force_y (force-in-dimension-on-body :y body-one body-two),
   ; :force_z (force-in-dimension-on-body :z body-one body-two)}))

(defn force-on-one-body [index, bodies]
  {:force_x 2.94E-10, :force_y -1.35E-10, :force_z 0})

(defn force-calculator [bodies]
      [{
       :force_x 2.94E-10, :force_y -1.35E-10, :force_z 0
       },
      {
       :force_x -2.94E-10, :force_y 1.35E-10, :force_z 0
      }])

(defn vector-difference [first-vector second-vector]
  {
    :x (dimensional-difference :x first-vector second-vector)
    :y (dimensional-difference :y first-vector second-vector)
    :z (dimensional-difference :z first-vector second-vector)
  })
