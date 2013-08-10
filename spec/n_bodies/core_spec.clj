(ns n-bodies.core-spec
  (:require [speclj.core :refer :all]
            [clojure.math.numeric-tower :refer [sqrt]]
            [n-bodies.core :refer [force-calculator,  
                                   dimensional-difference, 
                                   force-in-dimension-on-body,
                                   GRAVITY, 
                                   sum-of-forces-on-one-body,
                                   force-on-one-body-from-another]]))

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
        (should= {:force_x ( - (* GRAVITY (/ 8 9))), 
                  :force_y (* GRAVITY 8),
                  :force_z (* GRAVITY (/ 8 9)) 
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
          (should= {:force_x (- (* GRAVITY (/ 6 5))), 
                    :force_y (* GRAVITY (/ 10 3)),
                    :force_z (* GRAVITY (/ 15 8)) 
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

          (should= {:force_x (- (* GRAVITY (/ 6 5))), 
                    :force_y (* GRAVITY (/ 10 3)),
                    :force_z (* GRAVITY (/ 15 8))
                   },
                   (sum-of-forces-on-one-body one-body [other-body])))))

  ;(it "sums the forces on one body with 3 bodies total"
  ;  (let [one-body {
  ;                  :mass 5 ,
  ;                  :velocity {:x 20, :y 55, :z 0}
  ;                  :position {:x 2, :y 4, :z 4}}
  ;        second-body {
  ;                  :mass 6,
  ;                  :velocity {:x -20, :y 5, :z 0}
  ;                  :position {:x 7, :y 1, :z 0}}
  ;        third-body {
  ;                  :mass 2,
  ;                  :velocity {:x -20, :y 5, :z 0}
  ;                  :position {:x 2, :y 1, :z 0}}]

  ;        (should= {:force_x (- (* GRAVITY (/ 6 5))), 
  ;                  :force_y (* GRAVITY (/ 40 9)),
  ;                  :force_z (* GRAVITY (/ 5 2)) 
  ;                 },
  ;                 (sum-of-forces-on-one-body one-body [second-body third-body])))))

(describe "force-calculator"

  (it "outputs x, y, and z forces given two bodies"
    (let [bodies [{
                :mass 3,
                :velocity {:x 20, :y 55, :z 0}
                :position {:x 2, :y 5, :z 0}},
                {
                :mass 7,
                :velocity {:x -20, :y 5, :z 0}
                :position {:x 8, :y 3, :z 1}}]]

     (should= [{
                :force_x  (* (/ 7 -12) GRAVITY), :force_y (* (/ 21 4) GRAVITY), :force_z (* -21 GRAVITY)
               },
               {
                :force_x (* (/ 7 12) GRAVITY), :force_y (* (/ -21 4) GRAVITY), :force_z (* 21 GRAVITY)
               }], 
              (force-calculator bodies)))))

  ;(it "outputs x, y, and z forces given three bodies"
  ;    (let [bodies [{
  ;                :mass 3,
  ;                :velocity {:x 20, :y 55, :z 0}
  ;                :position {:x 2, :y 5, :z 0}},
  ;                {
  ;                :mass 2,
  ;                :velocity {:x -20, :y 5, :z 0}
  ;                :position {:x 8, :y 3, :z 1}}
  ;                {
  ;                :mass 5,
  ;                :velocity {:x 20, :y 55, :z 0}
  ;                :position {:x 1, :y 3, :z 2}}]]

  ;     (should= [{
  ;                :force_x  (* (/ 89 6) GRAVITY), :force_y (* (/ 21 4) GRAVITY), :force_z (* (/ -39 4) GRAVITY)
  ;               },
  ;               {
  ;                :force_x (* (/ 55 294) GRAVITY), :force_y (* (/ -4 3) GRAVITY), :force_z 0
  ;               }
  ;               {
  ;                :force_x (* (/ -745 49) GRAVITY), :force_y (* (/ -15 4) GRAVITY), :force_z (* (/ -25 4 GRAVITY))
  ;               }], 
  ;              (force-calculator bodies)))))

(run-specs)
