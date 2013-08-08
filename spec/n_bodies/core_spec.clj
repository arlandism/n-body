(ns n-bodies.core-spec
  (:require [speclj.core :refer :all]
            [clojure.math.numeric-tower :refer [sqrt]]
            [n-bodies.core :refer [force-calculator, vector-difference, 
                                   dimensional-difference, 
                                   force-on-one-body, force-in-dimension-on-body,
                                   GRAVITY, force-on-one-body-from-another]]))

(describe "dimensional-difference"
  (it "calculates the difference between two vector directions"
    (let [first-vector {:x 20, :y 55, :z 0},
          second-vector {:x -20, :y 5, :z 0}]
      (should= 50, (dimensional-difference :y first-vector second-vector))))

  (it "calculates the difference between two vector directions again"
    (let [first-vector {:x 20, :y 55, :z 0},
          second-vector {:x -20, :y 5, :z 0}]
      (should= 40, (dimensional-difference :x first-vector second-vector)))))

(describe "vector-difference"
  (it "calculates vector differences"
    (let [first-vector {:x 20, :y 55, :z 0},
          second-vector {:x -20, :y 5, :z 0}]
     (should= {:x 40, :y 50, :z 0}, (vector-difference first-vector second-vector))))

  (it "calculates vector differences with z"
    (let [first-vector {:x 37, :y 67, :z 23},
          second-vector {:x 49, :y 8, :z 0}]
       (should= {:x -12, :y 59, :z 23}, (vector-difference first-vector second-vector))))
  (it "calculates more vector differences"
    (let [first-vector {:x 7, :y 6, :z 0},
          second-vector {:x 49, :y 8, :z 0}]
       (should= {:x -42, :y -2, :z 0}, (vector-difference first-vector second-vector)))))

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


;(describe "force-on-one-body"
;  (context "two bodies"
;    (it "calculates the force exerted on one body given index & all bodies"
;     (let [bodies [{
;                  :mass 100,
;                  :velocity {:x 20, :y 55, :z 0}
;                  :position {:x 5, :y 42, :z 0}},
;                  {
;                  :mass 532,
;                  :velocity {:x -20, :y 5, :z 0}
;                  :position {:x 100, :y -2, :z 0}
;                  }]]
;        (should= 
;          {:force_x 2.94E-10, :force_y -1.35E-10, :force_z 0},
;          (force-on-one-body 0 bodies))
;
;        (should= 
;          {:force_x 2.94E-10, :force_y -1.35E-10, :force_z 0},
;          (force-on-one-body 1 bodies))))))

(describe "force-calculator"

  (it "outputs x, y, and z forces given two bodies"
    (let [bodies [{
                :mass 100,
                :velocity {:x 20, :y 55, :z 0}
                :position {:x 5, :y 42, :z 0}},
                {
                :mass 532,
                :velocity {:x -20, :y 5, :z 0}
                :position {:x 100, :y -2, :z 0}}]]

     (should= [{
                :force_x 2.94E-10, :force_y -1.35E-10, :force_z 0
               },
               {
                :force_x -2.94E-10, :force_y 1.35E-10, :force_z 0
               }], (force-calculator bodies)))))

  (it "outputs x, y, and z forces given some other stuff"
    (let [bodies [{
                :mass 100,
                :velocity {:x 20, :y 55, :z 0}
                :position {:x 5, :y 42, :z 0}},
                {
                :mass 532,
                :velocity {:x -20, :y 5, :z 0}
                :position {:x 100, :y -2, :z 0}}]]

     (should= [{
                :force_x 2.94E-10, :force_y -1.35E-10, :force_z 0
               },
               {
                :force_x -2.94E-10, :force_y 1.35E-10, :force_z 0
               }], (force-calculator bodies))))
;
;  (it "outputs x, y, and z forces given three bodies"
;    (let [bodies [{
;                    :mass 200,
;                    :velocity {:x 0, :y 0, :z 0}
;                    :position {:x 0, :y 0, :z 0}},
;                    {
;                    :mass 10,
;                    :velocity {:x 0, :y 120, :z 0}
;                    :position {:x 160, :y 0, :z 0}}
;                   {
;                    :mass 0.001,
;                    :velocity {:x 0, :y 53, :z 0}
;                    :position {:x 140, :y 0, :z 0}}]]
;         (should= [{
;                    :force_x 2.94E-10, :force_y -1.35E-10, :force_z 0
;                   },
;                   {
;                    :force_x -2.94E-10, :force_y 1.35E-10, :force_z 0
;                   }
;                   {
;                    :force_x -2.94E-10, :force_y 1.35E-10, :force_z 0
;                   }], (force-calculator bodies)))))
(run-specs)
