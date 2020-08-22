(ns zotero
  (:require [cheshire.core :as cheshire]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def all-cases (edn/read-string (slurp (io/resource "cases.edn"))))


(defn convert-icj-to-csl [m]
  (let [{:keys [title link culm type]} m]
    {:type "legal_case"
     :authority "ICJ"
     :title (str title ", " (case type
                              "Contentious" "Contentious"
                              "Advisory" "Advisory Opinion"))
     :issued {:date-parts [(str culm)]}
     :URL link}))

(->
  
  (map convert-icj-to-csl all-cases)
  (cheshire/generate-string {:pretty true})
  (->>
  (spit "cases-csl.json"))
  )





