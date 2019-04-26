(ns first.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:require [first.dynamic :as dynamic])
  (:require [genartlib.util :as u])
  (:gen-class))

(q/defsketch first-sketch
  :title "Testing 1 2"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup dynamic/setup
  ; update-state is called on each iteration before draw-state.
  :update dynamic/update-state
  :draw dynamic/draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn refresh []
  (use :reload 'first.dynamic)
  (.redraw first-sketch))

(defn get-applet []
  first-sketch)