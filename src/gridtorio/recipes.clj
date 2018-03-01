(ns gridtorio.recipes
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]))

;; TODO: sanity-check the recipes list
;;  - resin, aluminum plates, others have no ingredients
;;  - lithia-water/etc should be removed (produced by pumps)

(def recipes-filename "recipes.json")

(def items-to-produce
  #{:rocket-part :satellite
    :science-pack-1 :science-pack-2 :science-pack-3 :military-science-pack
    :high-tech-science-pack :production-science-pack :logistic-science-pack})

(defn add-weights [item]
  (let [total   (apply + (map :amount (:ingredients item)))
        weigher (fn [{:as ingredient :keys [amount]}]
                  (assoc ingredient :weight (/ amount total)))]
    (update item :ingredients (partial map weigher))))

(def recipes
  (let [raw-recipes (-> (io/resource recipes-filename)
                        slurp
                        (json/parse-string true))
        reducer     (fn [result k v]
                      (assoc result k (add-weights v)))]
    (reduce-kv reducer {} raw-recipes)))

(defn transitive-ingredients [recipes item-name]
  (loop [queued #{item-name}
         result #{}]
    (let [item        (first queued)
          ingredients (->> (recipes item)
                           :ingredients
                           (map :name)
                           (map keyword)
                           set)]
      (if item
        (recur (into (set (rest queued)) (set/difference ingredients result))
               (conj result item))
        result))))

(def factories
  (->> items-to-produce
       (map (partial transitive-ingredients recipes))
       (apply set/union)))

(def factories-matrix
  (reduce (fn [result factory]
            (let [to-name-weight-map (fn [result ingredient]
                                       (assoc result (keyword (:name ingredient)) (:weight ingredient)))
                  ingredients        (->> (recipes factory)
                                          :ingredients
                                          (reduce to-name-weight-map {}))]
              (assoc result factory ingredients)))
          {}
          factories))
