(ns pdfs
  (:require [pdfboxing.common :as pdf]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [pdfboxing.text :as pdf-text]
            [dk.ative.docjure.spreadsheet :as docjure]
            [cljfx.api :as fx])
  (:import [org.apache.pdfbox.pdmodel PDDocument PDPage]
           [org.apache.pdfbox.text PDFTextStripper PDFTextStripperByArea]
           [org.apache.pdfbox.rendering PDFRenderer]
           [java.util ArrayList]
           [java.awt.geom AffineTransform]
           [java.awt.image BufferedImage]))

(def pdf-path "/Users/orestis/Dropbox (Personal)/Phd case law tables/help Oresti xanomaste/A. M. Stuyt auth. Survey of International Arbitrations 1794–1938.pdf")


(defn page-seq [^PDDocument doc]
  (let [labels (-> doc
                   (.getDocumentCatalog)
                   (.getPageLabels)
                   (.getLabelsByPageIndices))]
    (for [idx (range 0 (.getNumberOfPages doc))]
      (let [sidx (inc idx)
            label (get labels idx)]
        {:idx0 idx
         :idx1 sidx
         :page (.getPage doc idx)
         :label label}))))


(defn page-text-seq [^PDDocument doc]
  (let [stripper (PDFTextStripper.)
        labels (-> doc
                   (.getDocumentCatalog)
                   (.getPageLabels)
                   (.getLabelsByPageIndices))]
    (for [idx (range 0 (.getNumberOfPages doc))]
      (let [sidx (inc idx)
            _ (doto stripper
                (.setStartPage sidx)
                (.setEndPage sidx))
            text (.getText stripper doc)
            label (get labels idx)]
        {:idx0 idx
         :idx1 sidx
         :text text
         :page (.getPage doc idx)
         :label label}))))


(defonce doc (pdf/load-pdf pdf-path))
(comment
  (def pages-with-text (page-text-seq doc))
  (nth pages-with-text 20)
  (def page (.getPage doc 19))
  (.getArtBox page))


(defn page-text-area [page rect]
  (let [stripper (doto (PDFTextStripperByArea.)
                   (.addRegion "region" rect)
                   (.extractRegions page))]
    (.getTextForRegion stripper "region")))

(defn page-text-regions [page regions]
  (let [stripper (PDFTextStripperByArea.)]
    (doseq [[k rect] regions]
      (.addRegion stripper (name k) rect))
    (.extractRegions stripper page)
    (into {}
          (for [k (keys regions)]
            [k (.getTextForRegion stripper (name k))]))))

(def regions
  {:case-nr (java.awt.Rectangle. 0 79 481.89 15.5)
   :parties (java.awt.Rectangle. 0 100 481.89 32)
   :numbers (java.awt.Rectangle. 0 90 81 591.9)})


(defn process-entire-doc [doc regions]
  (let [pages (page-text-seq doc)]
    (for [{:keys [page] :as p} pages]
      (let [r-texts (page-text-regions page regions)]
        (merge r-texts
               (select-keys p [:idx0 :idx1 :text :label]))))))


(defn is-new-case [{:keys [case-nr]}]
  (when-let [[_ n] (re-matches #"^Nr\. (\d+).*$" (str/trim case-nr))]
    n)) 

(is-new-case {:case-nr "Nr. 3 (conti) "})

(defn section-match [x]
  (let [r (re-pattern (str "^\\s*" x "\\s*?\\..*"))]
    #(re-matches r %)))

(defn extract-section [lines section-start next-section-start]
  (->> lines
       (drop-while (complement section-start))
       (take-while (complement next-section-start))))

(defn partition-section [lines markers ks]
  (let [keypairs (mapv vector markers ks)]
    (loop [[marker k] (first keypairs)
           keypairs (rest keypairs)
           acc {}
           lines (drop-while (complement marker) lines)]
      (if-let [[next-marker next-k] (first keypairs)]
        (let [[current-section new-lines] (split-with (complement next-marker) lines)] 
          ;(println lines new-lines current-section)
          (if (and (= lines current-section) (empty? new-lines))
            acc
            (recur [next-marker next-k]
                   (rest keypairs)
                   (assoc acc k (str/join "\n" current-section))
                   new-lines)))
        acc))))

(partition-section ["foo" "bar" "1. xxx" "bbb" "2. 555" "3. yyy" "zzz" "5. the end"]
                   (mapv section-match ["1" "2" "3" "5"])
                   [:1 :2 :3 :5])

(partition-section ["foo" "bar" "1. xxx" "zzz" "5. the end"]
                   (mapv section-match ["1" "2" "3" "5"])
                   [:1 :2 :3 :5])

(defn extract-section-4 [text]
  (let [lines (str/split-lines text)
        sections-to-split (mapv section-match ["4" "a" "b" "c" "5"])
        markers [:treaty :date :applicable-law :text :end]]
    (partition-section lines sections-to-split markers)))

(extract-section-4  "2 \nNr. 2 \n1. GREAT BRITAIN- UNITED STATES OF AMERICA. \n2. Recovery of debts . \n.,Whereas it is alleged by divers British merchants and others His Majesty's \nsubjects, that debts, to a considerable amount, which were bona fide contracted \nbefore the Peace, still remain owing to them by citizens or inhabitants of the \nUnited States, and that by the operation of various lawful impediments since \nthe peace, not only the full recovery of the said debts has been delayed, but \nalso the value and security thereof have been, in several instances, impaired and \nlessened, so that, by the ordinary course of judicial proceedings, the British \ncreditors cannot now obtain, and actually have and receive full and adequate \ncompensation for the losses and damages which they have thereby sustained \n... . For the purpose of ascertaining the amount of any such losses or damages, \nfive Commissioners shall be appointed, . . .. \" Article 6. \n3. Commission: Th. Macdonald (Gr.Br.); H. Pye Rich (Gr.Br.); Th. Fitz-\nsimons (U.S.A.); ]. Innes, succeeded by S. Sitgreaves (U.S.A.); ]. \nGuillemard (Gr.Br.). \n4. Jay Treaty, London. \na. November 19, 1794. \nb. The five Commissioners should take the following oath: \n., .... I will honestly, diligently, impartially and carefully examine, and \nto the best of my judgment, according to justice and equity, decide all such \ncomplaints, .... \" Article 6. \nc. La Fontaine 3; Lapradelle-P. 1-15; Malloy 1-590; de Martens R. (2nd ed.) 5-641; Moore 5-4720; State Papers \n1- 784. \n5. Settlement by Convention, London. \nArticle 6 of the Jay Treaty was annuled. \na. January 8, 1802. \nb. Great Britain. \nc. U.S.A. paid a sum of$ 2.664.000 (Moore 1-298). \nBy Act of April 22, 1803, a Domestic Commission (Th. Macdonald; H. Pye \nRich and J. Guillemard) was appointed for the adjudication of the claims \nand the distribution of the indemnity. \n8 Reports resulted (Moore Int. Adj . 3-375/433) . \nd. La Fontaine 4 ; Lapradelle·P. 1-27; Malloy 1-610; de Martens R. (2nd ed.) 7-397; Moore 5-4727; State Papers \n1-808. \ne. Am. State Papers F. R. vol. I and 2 passim; Lapradelle-P. 1-12/28; Moore 1- 271/298; Moore Int. Adj. vol. 3; \nR.D.l.L.C. 6 (1874)-118. \n")

(extract-section-4 "19 \nNr. 17 \n1. NASSAU - PRUSSIA. \n2. Cession of territory. \n,La partie de la principaute de Siegen et des bailliages de Burbach et de \nNeunkirchen, qui, d'apres !'article ci-dessus, devra etre cedee, sera determinee \npar des commissaires nommes par les deux H.P.C . ... . Les commissaires se con-\nformeront au principe de la contigulte de ces portions avec les territoires respec-\ntifs, et auront un soin particulier pour que Jes rapports communaux, ecclesiasti-\nq ues et industriels, actuellement existants, soient main tenus; so us les rapports \nindustriels sont specialement compris ceux qui regardent !'exploitation des \nmines . Dans le cas ou ces commissaires ne pourraient pas s'accorder sur l'un \nou I' autre de ces objets, ils sont autorises a compromettre sur un arbitre nomme \npar eux-memes, qui decidera sans autre recours.\" Article 3 . \n3. Commission: \n4. Treaty, Vienna. \n5. \na. May 31, 1815. \nb. (See sub 2.) \nc. Descamps-R. 1801-416; State Papers 2-102. \na. \nb. \nc. \nd. \ne. \n")

(defn locate-section-4 [cases-seq]
  (loop [page (first cases-seq)
         more (rest cases-seq)]
    (let [{:keys [numbers text]} page]
      #_(println "numbers" numbers)
      (if-not (re-find #"(?m)^\s*4\.\s*" numbers)
        (if (seq more)
          (recur (first more) (rest more))
          nil)
        (extract-section-4 text)))))

(re-find #"(?m)^\s*4\.\s*"  "2. \n3. \n4. \n5. \n")

(locate-section-4 [{:case-nr "Nr. 17 \n",
                    :parties "1. NASSAU - PRUSSIA. \n",
                    :numbers "2. \n3. \n4. \n5. \n",
                    :idx0 28,
                    :idx1 29,
                    :text
                    "19 \nNr. 17 \n1. NASSAU - PRUSSIA. \n2. Cession of territory. \n,La partie de la principaute de Siegen et des bailliages de Burbach et de \nNeunkirchen, qui, d'apres !'article ci-dessus, devra etre cedee, sera determinee \npar des commissaires nommes par les deux H.P.C . ... . Les commissaires se con-\nformeront au principe de la contigulte de ces portions avec les territoires respec-\ntifs, et auront un soin particulier pour que Jes rapports communaux, ecclesiasti-\nq ues et industriels, actuellement existants, soient main tenus; so us les rapports \nindustriels sont specialement compris ceux qui regardent !'exploitation des \nmines . Dans le cas ou ces commissaires ne pourraient pas s'accorder sur l'un \nou I' autre de ces objets, ils sont autorises a compromettre sur un arbitre nomme \npar eux-memes, qui decidera sans autre recours.\" Article 3 . \n3. Commission: \n4. Treaty, Vienna. \n5. \na. May 31, 1815. \nb. (See sub 2.) \nc. Descamps-R. 1801-416; State Papers 2-102. \na. \nb. \nc. \nd. \ne. \n",
                    :label "19"}])

(defn extract-citation [cases-seq]
  (let [first-page (first cases-seq)
        {:keys [case-nr parties label]} first-page
        section-4 (locate-section-4 cases-seq)]
    (merge section-4
           {:case-nr (str/trim case-nr)
            :parties (str/trim parties)
            :page-nr (str/trim label)})))

(comment
  (let [processed (process-entire-doc doc regions)
        s (with-out-str (pprint/pprint processed))]
    (spit "stuyt.edn" s)
    )

  (def processed (drop 10 (edn/read-string (slurp "stuyt.edn"))))
  (first processed)
  (->> processed
       (partition-by is-new-case)
       #_(take 20)
       (mapv extract-citation)
       #_(take 2)
       (def extracted)
       )

  (spit "stuyt-extracted.edn" (with-out-str (pprint/pprint extracted)))

  (-> "stuyt-extracted.edn"
       slurp
      (edn/read-string)
      #_(take 10)
      (extracted-to-excel "stuyt.xlsx"))


  )

(def excel-headers 
  [:parties
   :date
   :page-nr
   :case-nr
   :treaty
   :applicable-law
   :text])


(defn extracted-to-excel [treaties fname]
  (let [extr (apply juxt excel-headers)
        rows (->> treaties
                  (mapv extr))
        header (mapv name excel-headers)
        wb (docjure/create-workbook "Stuyt"
                                    (concat [header] rows))]
    (docjure/save-workbook! fname wb)))


(def ^:dynamic *dpi* 150)

(defn- create-image [rect]
  (let [scale (/ *dpi* 72)
        w (* scale (.width rect))
        h (* scale (.height rect)) ]
    (BufferedImage. w h BufferedImage/TYPE_INT_RGB)))

(defn- create-graphics [image rect]
  (let [scale (/ *dpi* 72)
        t (doto (AffineTransform/getScaleInstance scale scale)
            (.concatenate (AffineTransform/getTranslateInstance (- (.x rect))
                                                                (- (.y rect)))))]
    (doto (.createGraphics image)
      (.setBackground (java.awt.Color/WHITE))
      (.setTransform t))))

(defn render-page-rect [doc page-idx rect]
  (time
    (let [image (create-image rect)
          graphics (create-graphics image rect)
          renderer (PDFRenderer. doc)]
      (.renderPageToGraphics renderer page-idx graphics)
      (.dispose graphics)
      image)))

(defn image-to-fx [image]
  (javafx.embed.swing.SwingFXUtils/toFXImage image nil))





(defprotocol Renderer
  (render [this page-idx area]))

(defn cached-renderer [doc]
  (let [cache (atom {})]
    (reify 
      Renderer
      (render [_ page-idx area]
      (if-let [imgp (get-in @cache [page-idx area])]
        imgp
        (let [_ (println "cache miss" page-idx area)
              imgp (promise)]
          (swap! cache assoc-in [page-idx area] imgp)
          (future
            (deliver imgp (render-page-rect doc page-idx area)))
          imgp
          ))))))


(defn prefetching-renderer [doc lookahead]
  (let [crender (cached-renderer doc)
        max-page (.getNumberOfPages doc)
        render!
        (fn [cur-idx area]
          (let [[current & _]
                (doall
                  (for [i (range cur-idx (min (+ cur-idx lookahead) max-page))]
                    (render crender i area)))]
            current))]
    (reify
      Renderer
      (render [_ page-idx area]
        (render! page-idx area)))))



(def *state
  (atom {:current-page-idx 0
         :renderer  (prefetching-renderer doc 5)
         :all-pages (vec (page-seq doc))}))

(swap! *state assoc :current-page-idx 10)

(defmulti event-handler :event/type)


(defmethod event-handler ::prev-page [e]
  (swap! *state update :current-page-idx dec))

(defmethod event-handler ::next-page [e]
  (swap! *state update :current-page-idx inc))


(defn page-control-view [{{:keys [idx1 label]} :page}]
  {:fx/type :h-box
   :spacing 30
   :min-width :use-pref-size
   :children [{:fx/type :button
               :on-action {:event/type ::prev-page}
               :text "Prev"}
              {:fx/type :label
               :min-width :use-pref-size
               :text (str label " (" idx1 ")")}
              {:fx/type :button
               :on-action {:event/type ::next-page}
               :text "Next"}]})

(defn page-area-view [{{:keys [idx0]} :page
                       renderer :renderer
                       area :area}]
  {:fx/type :image-view
   :image (-> (render renderer idx0 area)
              (deref)
              (image-to-fx))})



(defn root-view [{{:keys [all-pages current-page-idx renderer]} :state}]
  (let [page (nth all-pages current-page-idx)
        regions-text (page-text-regions (:page page) regions)]
    {:fx/type :stage
     :showing true
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :spacing 10
                    :children 
                    [{:fx/type page-control-view
                      :page page}
                     {:fx/type :scroll-pane
                      :content {:fx/type :v-box
                                :children 
                                (vec
                                  (for [[key area] regions]
                                    {:fx/type :v-box
                                     :spacing 0
                                     :children 
                                     [{:fx/type :label
                                       :text (str key)}
                                      {:fx/type page-area-view
                                       :page page
                                       :renderer renderer
                                       :area area}
                                      {:fx/type :label
                                       :text (get regions-text key)}]}))}}]}}}))


(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type root-view
                                     :state state}))
    :opts {:fx.opt/map-event-handler event-handler}))

(comment
  (fx/mount-renderer *state renderer)
  (fx/unmount-renderer *state renderer)

  (renderer)
)
