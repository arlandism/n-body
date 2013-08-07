(ns n-bodies.core-spec
  (:require [speclj.core :refer :all]
            [n-bodies.core :refer :all]))

(describe "force-calculator"

  (it "outputs x, y, and z forces"
    (let [bodies [{:mass 100,
                :velocity {:x 20, :y 55}
                :position {:x 5, :y 42}},
               {:mass 532,
                :velocity {:x -20, :y 5}
                :position {:x 100, :y -2}}
               ]]
     (should= [{
                :force_x 2.94E-10, :force_y -1.35E-10,
               },
               {
                :force_x -2.94E-10, :force_y 1.35E-10,
               }], (force-calculator bodies))))

)
(run-specs)
