(ns first.dynamic
  (:require [quil.core :as q])
  (:require [genartlib.util :as u])
  (:require [genartlib.random :refer :all])
  (:require [genartlib.algebra :refer :all])
  (:require [clojure.math.numeric-tower :refer :all]))

(def num-points "number of points" 29)
(def r-min "min circle size" 10)
(def r-max "max circle size" 60)
(def speed-mean "" 0)
(def speed-variance "" 2)
(def max-hue-rate "" (/ 255 45)) ; hue change per frame, a full cycle around the color wheel takes minimum 1.5 seconds

(defonce hue-adj-samples (atom []))

(defonce play-state (atom {:physics false
                           :color false
                           :all false}))

(defn play-pause [k] 
  (swap! play-state #(update-in % [k] not)))

(defn weighted-avg
  ([xs weights]
    (/ (reduce + (map * xs weights))
       (min (count weights) (count xs)))))

(defn plot [f xs]
  (doseq [x xs]
    (q/point x (f x))))

(defn plot-points [ps]
  (doseq [[x y] ps]
    (q/point x y)))

(defn hue-to-rad [hue]
  (/ (* q/PI hue) 128))

(defn hue-distance [h1 h2]
  (mod (- h1 h2) 255))

(defn in-range [x start end]
  (and (< start x) (< x end)))

(defn diff-from-opp-hue [h0 h1]
  (let [target-hue (mod (+ h1 128) 255)
        diff (- target-hue h0)]
    #_(when (< diff -128)
      (println (format "%1$.0f" h0) (format "%1$.0f" h1)))
    (cond ; There's probably a more natural way to do this with mod
      (> diff 128) (- diff 256)
      (< diff -128) (+ diff 256)
      :else diff)))

(defn logistic-curve [max-val growth mid-x]
  #(/ max-val (+ 1 (expt Math/E (- (* growth (- % mid-x)))))))

(defn reverse-logistic-curve [max-val growth mid-x]
  ;reflected across x axis
  #((logistic-curve max-val growth (- mid-x)) (- %)))

(defn adjust-hue [target points]
  (let [{[x0 y0] :p hue0 :hue} target
        other-points (filter #(not= target %) points)
        distances (map (fn [{[x1 y1] :p}] (q/dist x0 y0 x1 y1)) other-points)
        hue-diffs (map #(diff-from-opp-hue hue0 (:hue %)) other-points)
        
        smoothed-distances (map (reverse-logistic-curve max-hue-rate 0.015 50) distances)
        
        adjustment (reduce + (map * hue-diffs smoothed-distances))
        scaled-adjustment (rescale adjustment
                                   -128 128
                                   (- max-hue-rate) max-hue-rate)]
    (when (< 2000 (Math/abs adjustment))
      #_(println hue-diffs)
      #_(println (map #(into [] %1 %2) (map :hue other-points) hue-diffs)))
    
    (when (< (q/random 100) 0.9)
      #_(println "hue0" (format "%1$.0f" hue0) \tab \tab 
               "adjustment" (format "%1$.0f" adjustment) \tab
               "scaled-adjustment" (format "%1$.2f" scaled-adjustment))
      (swap! hue-adj-samples #(cons [hue0 scaled-adjustment] %)))
    scaled-adjustment))

(defn update-physics [point]
  (let [{p :p v :v hue :hue} point
        [x0 y0] p
        [angle speed] v
        dx (* speed (q/cos angle))
        dy (* speed (q/sin angle))
        x1 (+ x0 dx)
        y1 (+ y0 dy)]
    {:p [x1 y1]
     :v [(let [flip-y (not (in-range y1 0 (u/h)))
               flip-x (not (in-range x1 0 (u/w)))]
           (cond
             (and flip-x flip-y) (+ q/PI angle)
             flip-x (- q/PI angle)
             flip-y (- q/TWO-PI angle)
             :else angle))
         speed]}))

(defn update-points [points]
  "calculate new point positions based on update rule"
  (map (fn [point]
         (let [{p :p v :v} (if (:physics @play-state) (update-physics point) point)
               hue-nudge (if (:color @play-state) (adjust-hue point points) 0)]
           {:p p
            :v v
            :hue (mod (+ hue-nudge (:hue point)) 255)
            :hue-speed (+ (* 0.85 (:hue-speed point)) 
                          (* 0.15 (Math/abs hue-nudge)))}))
       points))

(defn initial-state []
  (let [[xc yc] [(/ (u/w) 2) (/ (u/h) 2)]
        dangle (/ q/TWO-PI num-points)]
    {:points (map (fn [i] 
                    (let [angle (* i dangle) 
                          x (+ xc (* 150 (q/cos angle)))
                          y (+ yc (* 150 (q/sin angle)))] 
                      {:p [x y]
                       :v [(q/random q/TWO-PI) (gauss speed-mean speed-variance)]
                       :hue (q/random 255)
                       :hue-speed 0})) 
                  (range num-points))}))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/smooth)

  (initial-state))

(defonce reset-state? (atom false))

(defn reset-state []
  (swap! reset-state? (fn [_] true)))

(defn update-state [{points :points}]
  (if @reset-state?
    (do
      (swap! reset-state? (fn [_] false))
      (initial-state))
    {:points (if (:all @play-state) 
               (update-points points)
               points)}))

(defn draw-state [state]
  (q/background 60)
  (q/stroke 255)
  
  (doseq [point (:points state)]
    (let [{[x y] :p [angle speed] :v hue :hue hue-speed :hue-speed} point]
      (let [r (rescale hue-speed 
                       0 max-hue-rate
                       r-min r-max)
            saturation (rescale hue-speed
                                0 max-hue-rate
                                160 255)]
        (q/fill hue saturation 255)
        (q/stroke hue saturation 255)
        (q/ellipse x y r r)))))
  
  
