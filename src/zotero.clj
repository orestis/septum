(ns zotero
  (:require [cheshire.core :as cheshire]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(slurp "resources/cases.edn")

(def all-cases (edn/read-string (slurp (io/resource "cases.edn"))))
(def pcij-series-a (edn/read-string (slurp (io/resource "pcij-series-a.edn"))))


(defn convert-icj-to-csl [m]
  (let [{:keys [title link culm type]} m]
    {:type "legal_case"
     :authority "ICJ"
     :title (str title ", " (case type
                              "Contentious" "Contentious"
                              "Advisory" "Advisory Opinion"))
     :issued {:date-parts [(str culm)]}
     :URL link}))

(defn convert-pcij-case-to-csl [m]
  (let [{:keys [title id docs]} m]
    (for [{doc-title :title link :link} docs]
      {:type "legal_case"
       :authority "PCIJ"
       :title (str id ": " title ", " doc-title)
       :URL link})))

(comment

  (->
    (mapcat convert-pcij-case-to-csl pcij-series-a)
    (cheshire/generate-string {:pretty true})
    (->> (spit "pcij-series-a-csl.json")))

  (->
    (map convert-icj-to-csl all-cases)
    (cheshire/generate-string {:pretty true})
    (->>
      (spit "cases-csl.json"))))





