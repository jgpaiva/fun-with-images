(ns fun-with-images.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn to-angle [[x y]]
  (let [new-x (- x (/ (q/width) 2))
        new-y (- y (/ (q/height) 2))]
    (mod (* 1 (* 255 (/ (Math/atan2 new-y new-x) (* 2 Math/PI)))) 255)))

(defn dist [[x1 y1] [x2 y2]]
  (Math/sqrt
	(+
	  (Math/pow (- x1 x2) 2)
	  (Math/pow (- y1 y2) 2))))

(defn dist-to-center [point]
  (* (dist point [(/ (q/width) 2)
                  (/ (q/height) 2)])
     2.55))

(defn in-boundaries [{:keys [max-x max-y]} [x y]]
  (and (>= x 0)
       (> max-x x)
       (>= y 0)
       (> max-y y)))

(defn gen-directions [[x y] board]
  (->> [[x (inc y)] [(inc x) y] [x (dec y)] [(dec x) y]]
    (filter (partial in-boundaries board))
    (into [])))

(defn walk [point board]
  (let [directions (gen-directions point board)]
    (rand-nth directions)))

(defn walk-target [target point board]
  (let [directions (gen-directions point board)
        directions-sorted (sort-by (partial dist target) directions)]
    (first directions-sorted)))

(defn prob-walk-target [prob target point board]
  (if (< prob (rand))
    (walk point board)
    (walk-target target point board)))

(comment (defn lots-of-walk [point board f]
  (let [next (f point board)]
    (lazy-seq (cons next (lots-of-walk next board f))))))

(defn two-walks [point1 point2 board f]
  (let [next1 (f point2 point1 board)
        next2 (f point1 point2 board)]
    (lazy-seq (cons [next1 next2] (two-walks next1 next2 board f)))))

(defn not-close-enough [p1 p2] (> (dist p1 p2) 2))

(defn take-while+
  [pred coll]
  (lazy-seq
    (when-let [[f & r] (seq coll)]
      (if (pred f)
        (cons f (take-while+ pred r))
        [f]))))

(defn draw []
  (q/background 255)
  (q/frame-rate 0.001)
  (q/stroke-weight 3)
  (q/color-mode :hsb)
  (doseq [prob (take 50 (iterate (partial * 0.95) 0.6))]
    (let [start1 [100 200]
          start2 [700 200]
          color1 200
          color2 (- color1 180)
          walk-function (partial prob-walk-target prob)
          board {:max-x (q/width)
                 :max-y (q/height)}]
      (doseq [[point1 point2] (take-while+
                                (fn [[p1 p2]] (not-close-enough p1 p2))
                                (take 100000
                                      (two-walks start1 start2 board walk-function)))]
        (q/stroke (/ (* 255 color1) 360) (+ 20 (* prob 255)) 255)
        (apply q/point point1)
        (q/stroke (/ (* 255 color2) 360) (+ 20 (* prob 255)) 255)
        (apply q/point point2))))
  (q/save "/mnt/c/Users/João/Desktop/test.png"))

(q/sketch
  :size [800 400]
  :draw draw
  :features [:exit-on-close])


