(ns gridtorio.core
  (:require [gridtorio.recipes :as recipes]
            [clojure.pprint :refer [pprint]]
            [darwin.core :as darwin]
            [clojure.set :as set])
  (:import (java.lang.Math))
  (:gen-class))

(def factories recipes/factories-matrix)

(def grid-size (-> (count factories)
                   Math/sqrt
                   Math/ceil
                   int))

(defn taxicab-distance [[y1 x1] [y2 x2]]
  (+ (Math/abs (- y2 y1))
     (Math/abs (- x2 x1))))

(def grid-locations
  (for [x (range grid-size)
        y (range grid-size)]
    [x y]))

(defn random-grid []
  (let [positions (shuffle grid-locations)
        names     (keys factories)]
    (zipmap names positions)))

(defn score-grid-single-item [grid [item loc]]
  (let [weights               (factories item)
        to-weighted-distances (fn [[ingredient weight]]
                                (let [ingredient-loc (grid ingredient)]
                                  (double (* weight (taxicab-distance loc ingredient-loc)))))]
    (apply + (map to-weighted-distances weights))))

(defn score-grid [grid]
  (apply + (map (partial score-grid-single-item grid) grid)))

(defn mutate-grid [grid]
  (let [swap (fn [grid] (let [[k1 k2] (take 2 (shuffle (keys grid)))]
                         (assoc grid k1 (grid k2) k2 (grid k1))))]
    (last (take (inc (rand-int grid-size)) (iterate swap grid)))))

(defn fill-abandoned [grid abandoned]
  (let [all-positions   (set grid-locations)
        used-positions  (set (vals grid))
        avail-positions (shuffle (set/difference all-positions used-positions))]
    (merge grid
           (zipmap abandoned avail-positions))))

(defn crossover-grids* [grid1 grid2]
  (loop [remaining       (keys grid1)
         last-from-grid1 false
         result          {}
         positions       {}
         abandoned       []]
    (if (empty? remaining)
      (fill-abandoned result abandoned)
      (let [factory   (first remaining)
            position1 (get (if last-from-grid1 grid2 grid1) factory)
            position2 (get (if last-from-grid1 grid1 grid2) factory)
            position  (cond (nil? (positions position1)) position1
                            (nil? (positions position2)) position2
                            :else                        :nothing-good)]
        (if (= :nothing-good position)
          (recur (rest remaining) last-from-grid1 result positions (conj abandoned factory))
          (recur (rest remaining)
                 (not last-from-grid1)
                 (assoc result factory position)
                 (assoc positions position factory)
                 abandoned))))))


(defn crossover-grids [grid1 grid2]
  [(crossover-grids* grid1 grid2)
   (crossover-grids* grid2 grid1)])

;; (def result
;;   (let [raw (darwin/evolve score-grid
;;                            crossover-grids
;;                            mutate-grid
;;                            random-grid 500)]
;;     (first (sort-by :score (:rabble raw)))))

(defn -main
  []
  (let [result (darwin/evolve score-grid
                              crossover-grids
                              mutate-grid
                              random-grid 1000)]
    (pprint (first (sort-by :score (:rabble result))))))
