(ns n-bodies.core-spec
  (:require [speclj.core :refer :all]
            [clojure.math.numeric-tower :refer [ceil, sqrt, expt]]
            [n-bodies.core :refer :all]))

(describe "compute-f-on-dimensions"
  (it "performs given functions for all dimensions"
    (should= {:x 2 :y 1 :z 0} (compute-f-on-dimensions + {:x 0 :y 0 :z 0} {:x 2 :y 1 :z 0}))
    (should= {:x -2 :y -1 :z 0} (compute-f-on-dimensions - {:x 0 :y 0 :z 0} {:x 2 :y 1 :z 0}))
    (should= {:x 0 :y 0 :z 0} (compute-f-on-dimensions * {:x 0 :y 0 :z 0} {:x 2 :y 1 :z 0}))))

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
               (calculate-constant body-one body-two)))))

(describe "force-on-one-body-from-another"
  (it "can be used to calculate force on one body in all dimensions" 
    (let [one-body {
                   :mass 2 ,
                   :position {:x 2, :y 2 :z 2}}
          other-body {
                :mass 1,
                :position {:x 0 :y 0 :z 0 }}]
      (should= {:x (/ (* GRAVITY 2 -2) (expt (sqrt 12) 3)) , 
                :y (/ (* GRAVITY 2 -2) (expt (sqrt 12) 3)),
                :z (/ (* GRAVITY 2 -2) (expt (sqrt 12) 3)),
               }, 
               (force-on-one-body-from-another one-body other-body))))

  (it "can be used to calculate force on one body in all dimensions" 
    (let [one-body   {
                     :mass 1 ,
                     :position {:x 0, :y 1, :z 2}
                     }
          other-body {
                    :mass 2,
                    :position {:x 2, :y 1, :z 0}
                     }
         ]
      (should= {:x (/ (* GRAVITY 2 2) (expt (sqrt 8) 3)), 
                :y 0.0,
                :z  (/ (* GRAVITY 2 -2) (expt (sqrt 8) 3))
               } 
               (force-on-one-body-from-another one-body other-body)))))

(describe "sum-of-forces-on-one-body"
  (it "sums the forces on one body with 2 bodies total"
    (let [one-body   {
                      :mass 3 
                      :position {:x 2, :y 4, :z 4}
                     }
          other-body {
                      :mass 2,
                      :position {:x 7, :y 1, :z 0}
                     }
        ]

          (should= {:x (/ (* GRAVITY 6 5) (expt (sqrt 50) 3)) 
                    :y (/ (* GRAVITY 6 -3) (expt (sqrt 50) 3))
                    :z (/ (* GRAVITY 6 -4) (expt (sqrt 50) 3))
                   }
                   (sum-of-forces-on-one-body one-body [other-body]))

          (should= {:x (/ (* GRAVITY 6 -5) (expt (sqrt 50) 3)) 
                    :y (/ (* GRAVITY 6 3) (expt (sqrt 50) 3))
                    :z (/ (* GRAVITY 6 4) (expt (sqrt 50) 3))
                   }
                   (sum-of-forces-on-one-body other-body [one-body]))))

  (it "sums the forces on one body with 3 bodies total"
    (let [one-body    {
                       :mass 2
                       :position {:x 2, :y 4, :z 4}
                      } 
          second-body {
                      :mass 1
                      :position {:x 7, :y 1, :z 0}
                      }  
          third-body  {
                      :mass 2
                      :position {:x 2, :y 2, :z 0}
                      }
        ] 

          (should= {:x (+ (/ (* GRAVITY 2 5) (expt (sqrt 50) 3)))  
                    :y (+ (/ (* GRAVITY 2 -3) (expt (sqrt 50) 3)) (/ (* GRAVITY 4 -2) (expt (sqrt 20) 3)))
                    :z (+ (/ (* GRAVITY 2 -4) (expt (sqrt 50) 3)) (/ (* GRAVITY 4 -4) (expt (sqrt 20) 3)))
                   }
                   (sum-of-forces-on-one-body one-body [second-body third-body])))))

(describe "compute-forces"
  (it "outputs x, y, and z forces given two bodies"
    (let [bodies [{
                    :mass 3,
                    :position {:x 2, :y 5, :z 0}},
                  {
                    :mass 7,
                    :position {:x 8, :y 3, :z 1}
                  }]
         ]

     (should= [
               (sum-of-forces-on-one-body (first bodies) [(second bodies)])
               (sum-of-forces-on-one-body (second bodies) [(first bodies)]) 
              ]
              (compute-forces bodies))))

  (it "outputs x, y, and z forces given three bodies"
      (let [bodies [{
                      :mass 3,
                      :position {:x 2, :y 5, :z 0}
                    }
                    {
                      :mass 1,
                      :position {:x 1, :y 3, :z 2}
                    }
                    {
                      :mass 2,
                      :position {:x 1, :y 3, :z 1}
                    }]
          ]
       (should= [
                 (sum-of-forces-on-one-body (bodies 0) [(bodies 1) (bodies 2)])
                 (sum-of-forces-on-one-body (bodies 1) [(bodies 0) (bodies 2)]) 
                 (sum-of-forces-on-one-body (bodies 2) [(bodies 0) (bodies 1)])
                ] 
                (compute-forces bodies)))))

(run-specs)
