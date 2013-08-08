(ns n-bodies.core-spec
  (:require [speclj.core :refer :all]
            [n-bodies.core :refer [force-calculator, vector-calculator, directional-calculator]]))

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

;  (it "outputs x, y, and z forces given some other stuff"
;    (let [bodies [{
;                :mass 100,
;                :velocity {:x 20, :y 55, :z 0}
;                :position {:x 5, :y 42, :z 0}},
;                {
;                :mass 532,
;                :velocity {:x -20, :y 5, :z 0}
;                :position {:x 100, :y -2, :z 0}}]]
;
;     (should= [{
;                :force_x 2.94E-10, :force_y -1.35E-10, :force_z 0
;               },
;               {
;                :force_x -2.94E-10, :force_y 1.35E-10, :force_z 0
;               }], (force-calculator bodies))))
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

(describe "vector-calculator"
  (it "calculates vector differences"
    (let [vectors [
                {:x 20, :y 55, :z 0},
                {:x -20, :y 5, :z 0}]]
     (should= [:x 40, :y 50, :z 0], (vector-calculator vectors))))

(it "calculates vector differences"
    (let [vectors [
                {:x 37, :y 67, :z 0},
                {:x 49, :y 8, :z 0}]]
     (should= [:x 12, :y 59, :z 0], (vector-calculator vectors)))))

(describe "directional-calculator"
  (it "calculates the difference between two vector directions"
    (let [vectors [
                {:x 20, :y 55, :z 0},
                {:x -20, :y 5, :z 0}]]
      (should= 50, (directional-calculator :y vectors))))

   (it "calculates the difference between two vector directions again"
    (let [vectors [
                {:x 20, :y 55, :z 0},
                {:x -20, :y 5, :z 0}]]
      (should= 40, (directional-calculator :x vectors)))))
  
(run-specs)
