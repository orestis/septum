(ns septum
  (:require [hickory.core :as h]
            [hickory.select :as s]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clj-http.client :as client]))


(def base-url "https://www.icj-cij.org")
(def all-cases-url "https://www.icj-cij.org/en/list-of-all-cases")
(def all-cases-source (:body (client/get all-cases-url)))
(def all-cases-hickory
  (-> 
    (h/parse all-cases-source)
    (h/as-hickory)))

(defn all-child-text [node]
  (let [strings (atom [])
        children (:content node)]
    (doseq [c children]
      (cond
        (string? c) (swap! strings conj c)
        (map? c) (swap! strings (fn [v] (vec (concat v (all-child-text c)))))))
    @strings))



(defn content [node]
  (-> node
      (all-child-text)
      (->> (str/join ""))
      (str/trim)))

(defn tr-to-case [tr]
  (try
    (let [a (first (s/select (s/tag :a) tr))
          [intro culm] (s/select (s/child (s/tag :td) (s/tag :h5)) tr)
          t (first (s/select (s/tag :h6) tr))]
      {:title (content a) 
       :link (->> a :attrs :href (str base-url))
       :intro (content intro)
       :culm (content culm)
       :type (content t)})
    (catch Exception _e
      (throw (ex-info "Cannot parse tr" {:tr tr})))))


(def all-cases
  (->>
    all-cases-hickory
    (s/select (s/tag :tr))
    (filter #(nil? (:attrs %)))
    (drop 1)
    (map tr-to-case)))

(comment
  (spit "cases.edn" (with-out-str (pprint/pprint all-cases)))
  )


