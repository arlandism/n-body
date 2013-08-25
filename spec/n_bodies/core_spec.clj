(ns n-bodies.core-spec
  (:require [speclj.core :refer :all]
            [clojure.math.numeric-tower :refer [ceil, sqrt, expt, abs ]]
            [n-bodies.core :refer :all]))

(describe "distance-formula"
  (it "performs the distance formula for 3 dimensions"
    (should= (sqrt 2) (distance-formula {:x 1 :y 2 :z 0} {:x 2 :y 3 :z 0}))
    (should= (sqrt 13)(distance-formula {:x 1 :y 5 :z 4} {:x 1 :y 2 :z 6}))))

(describe "calculate-constant"
  (it "calculates the (product-of-masses * gravity/ (distance-form cubed)"    
    (let [mass-one 1 mass-two 2
          position-one {:x 2 :y 3 :z 1} position-two {:x 2 :y 3 :z 2} 
          body-one {:mass mass-one :position position-one} body-two {:mass mass-two :position position-two}]

      (should= (/ 
                 (* mass-one mass-two GRAVITY) 
                 (expt (distance-formula position-one position-two) 3)) 
               (calculate-constant body-one body-two)))

    (let [mass-one 3 mass-two 4
          position-one {:x 0 :y 0 :z 0} position-two {:x 0 :y 0 :z 0} 
          body-one {:mass mass-one :position position-one} body-two {:mass mass-two :position position-two}]

      (should= 0 (calculate-constant body-one body-two)))))
  
(describe "dimensional-difference"
  (it "calculates the difference between two vector directions"
    (let [first-vector {:x 20, :y 55, :z 0},
          second-vector {:x -20, :y 5, :z 0}]
      (should= 50, (dimensional-difference :y first-vector second-vector))))

  (it "calculates the difference between two vector directions again"
    (let [first-vector {:x 20, :y 55, :z 0},
          second-vector {:x -20, :y 5, :z 0}]
      (should= 40, (dimensional-difference :x first-vector second-vector)))))

(describe "force-in-dimension-on-body"
  (context "x dimension"
    (it "calculates force on one body in one dimension given dimension,body,other"
      (let [one-body {
                     :mass 2 ,
                     :velocity {:x 20, :y 55, :z 0}
                     :position {:x 3, :y 4, :z 0}}
            other-body {
                  :mass 64,
                  :velocity {:x -20, :y 5, :z 0}
                  :position {:x -1, :y -2, :z 0}}]
        (should= (* GRAVITY 8 ), (force-in-dimension-on-body :x one-body other-body)))))

  (context "y dimension"
    (it "calculates force on one body in one dimension given dimension,body,other"
      (let [one-body {
                     :mass 10 ,
                     :velocity {:x 20, :y 55, :z 0}
                     :position {:x 6, :y 4, :z 0}}
            other-body {
                  :mass 216,
                  :velocity {:x -20, :y 5, :z 0}
                  :position {:x 3, :y -2, :z 0}}]
        (should= (* GRAVITY 60) , (force-in-dimension-on-body :y one-body other-body)))))

  (context "z dimension"
      (it "calculates force on one body in one dimension given dimension,body,other"
        (let [one-body {
                       :mass 10 ,
                       :velocity {:x 20, :y 55, :z 0}
                       :position {:x 6, :y 4, :z 55}}
              other-body {
                    :mass 8,
                    :velocity {:x -20, :y 5, :z 0}
                    :position {:x 3, :y -2, :z 57}}]
          (should= (- (* GRAVITY 20)) , (force-in-dimension-on-body :z one-body other-body))))))
          
(describe "force-on-one-body-from-another"
  (it "can be used to calculate force on one body in all dimensions" 
      (let [one-body {
                     :mass 2 ,
                     :velocity {:x 20, :y 55, :z 0}
                     :position {:x 0, :y 3, :z 4}}
            other-body {
                  :mass 4,
                  :velocity {:x -20, :y 5, :z 0}
                  :position {:x 3, :y 2, :z 1}}]
        (should= {:x ( - (* GRAVITY (/ 8 9))), 
                  :y (* GRAVITY 8),
                  :z (* GRAVITY (/ 8 9)) 
                 }, 
                 (force-on-one-body-from-another one-body other-body))))

  (it "can be used to calculate force on one body in all dimensions" 
        (let [one-body {
                       :mass 5 ,
                       :velocity {:x 20, :y 55, :z 0}
                       :position {:x 2, :y 4, :z 4}}
              other-body {
                    :mass 6,
                    :velocity {:x -20, :y 5, :z 0}
                    :position {:x 7, :y 1, :z 0}}]
          (should= {:x (- (* GRAVITY (/ 6 5))), 
                    :y (* GRAVITY (/ 10 3)),
                    :z (* GRAVITY (/ 15 8)) 
                   }, 
                   (force-on-one-body-from-another one-body other-body)))))

(describe "sum-of-forces-on-one-body"
  (it "sums the forces on one body with 2 bodies total"
    (let [one-body {
                    :mass 5 ,
                    :velocity {:x 20, :y 55, :z 0}
                    :position {:x 2, :y 4, :z 4}}
          other-body {
                    :mass 6,
                    :velocity {:x -20, :y 5, :z 0}
                    :position {:x 7, :y 1, :z 0}}]

          (should= {:x (- (* GRAVITY (/ 6 5))), 
                    :y (* GRAVITY (/ 10 3)),
                    :z (* GRAVITY (/ 15 8))
                   },
                   (sum-of-forces-on-one-body one-body [other-body]))

          (should= {:x (* GRAVITY (/ 6 5)), 
                    :y (* GRAVITY (/ 10 -3)),
                    :z (* GRAVITY (/ 15 -8))
                   },
                   (sum-of-forces-on-one-body other-body [one-body]))))

  (it "sums the forces on one body with 3 bodies total"
    (let [one-body {
                    :mass 5 ,
                    :velocity {:x 20, :y 55, :z 0}
                    :position {:x 2, :y 4, :z 4}} 
          second-body {
                    :mass 6,
                    :velocity {:x -20, :y 5, :z 0}
                    :position {:x 7, :y 1, :z 0}}  
          third-body {
                    :mass 2,
                    :velocity {:x -20, :y 5, :z 0} 
                    :position {:x 2, :y 2, :z 0}}] 

          (should= {:x (- (* GRAVITY (/ 6 5))), 
                    :y (* GRAVITY (/ 35 6)),
                    :z (* GRAVITY (/ 5 2)) 
                   },
                   (sum-of-forces-on-one-body one-body [second-body third-body])))))
  
(describe "compute-forces"
  (it "outputs x, y, and z forces given two bodies"
    (let [bodies [{
                :mass 3,
                :velocity {:x 20, :y 55, :z 0}
                :position {:x 2, :y 5, :z 0}},
                {
                :mass 7,
                :velocity {:x -20, :y 5, :z 0}
                :position {:x 8, :y 3, :z 1}}]]

     (should= [
               (sum-of-forces-on-one-body (first bodies) [(second bodies)])
               (sum-of-forces-on-one-body (second bodies) [(first bodies)]) 
              ]
              (compute-forces bodies))))

  (it "outputs x, y, and z forces given three bodies"
      (let [bodies [{
                  :mass 3,
                  :velocity {:x 20, :y 55, :z 0}
                  :position {:x 2, :y 5, :z 0}},
                  {
                  :mass 5,
                  :velocity {:x 20, :y 55, :z 0}
                  :position {:x 1, :y 3, :z 2}}
                  {
                  :mass 2,
                  :velocity {:x -20, :y 5, :z 0}
                  :position {:x 8, :y 3, :z 1}}]]
       (should= [
                 (sum-of-forces-on-one-body (bodies 0) [(bodies 1) (bodies 2)])
                 (sum-of-forces-on-one-body (bodies 1) [(bodies 0) (bodies 2)]) 
                 (sum-of-forces-on-one-body (bodies 2) [(bodies 0) (bodies 1)])
                ] 
                (compute-forces bodies)))))
(describe "scale vector"
  (it "doubles the vector"
      (should= {:x 2 :y 4 :z 2} (scale-vector 2 {:x 1 :y 2 :z 1}))))

(run-specs)
