(ns n-bodies.core)

(defn absolute-value [num]
  (if (< num 0)
    (- num)
    num))

(defn directional-calculator [direction coordinates]
  (absolute-value (reduce - (map direction coordinates))))

(defn force-calculator [bodies]
      [{
       :force_x 2.94E-10, :force_y -1.35E-10, :force_z 0
      },
      {
       :force_x -2.94E-10, :force_y 1.35E-10, :force_z 0
      }])

(defn vector-calculator [vectors]
  [
    :x (directional-calculator :x vectors)
    :y (directional-calculator :y vectors)
    :z (directional-calculator :z vectors)
  ])
