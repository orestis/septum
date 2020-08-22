(ns septum
  (:require [hickory.core :as h]
            [hickory.select :as s]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clj-http.client :as client]))


(def base-url "https://www.icj-cij.org")
(def all-cases-url "https://www.icj-cij.org/en/list-of-all-cases")

(defn fetch-and-parse [url]
  (->
    (client/get url)
    :body
    h/parse
    h/as-hickory))


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



(comment
  (def all-cases
    (->>
      (fetch-and-parse all-cases-url)
      (s/select (s/tag :tr))
      (filter #(nil? (:attrs %)))
      (drop 1)
      (map tr-to-case)))
  (spit "cases.edn" (with-out-str (pprint/pprint all-cases)))
  )

(def pcij-series-urls 
  {:series-a "https://www.icj-cij.org/en/pcij-series-a"
   :series-b "https://www.icj-cij.org/en/pcij-series-b"
   :series-ab "https://www.icj-cij.org/en/pcij-series-ab"})

(defn pcij-title [node]
  (let [without-id 
        (->> node
            :content
            (remove (fn [n] (and (map? n) (= :small (:tag n))))))]
    (content {:content without-id})))
      


(defn pcij-div-to-case [node]
  (let [title (first (s/select (s/tag :h4) node))
        docs (s/select (s/child (s/tag :h6) (s/tag :a)) node)]
    {:title (pcij-title title)
     :id (-> title :attrs :id)
     :docs (mapv (fn [d]
                   {:title (content d)
                    :link (-> d :attrs :href (->> (str base-url)))}) docs) }))


(defn pcij-process [url]
  (->> url
       fetch-and-parse
       (s/select (s/child (s/class "fadein-3") (s/tag :div)))
       (map pcij-div-to-case)))


(comment

  
  (doseq [[k url] pcij-series-urls]
    (-> url
        pcij-process
        pprint/pprint
        with-out-str
        (->> (spit (str "pcij-" (name k) ".edn")))
        ))

  )

