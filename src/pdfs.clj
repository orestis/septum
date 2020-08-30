(ns pdfs
  (:require [pdfboxing.common :as pdf]
            [clojure.string :as str]
            [pdfboxing.text :as pdf-text]
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
         :label label}))))


(defonce doc (pdf/load-pdf pdf-path))
(comment
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


(def regions
  {:case-nr (java.awt.Rectangle. 0 79 481.89 15.5)
   :parties (java.awt.Rectangle. 0 100 481.89 32)})

(defn root-view [{{:keys [all-pages current-page-idx renderer]} :state}]
  (let [page (nth all-pages current-page-idx)
        regions-text (page-text-regions (:page page) regions)]
    {:fx/type :stage
     :showing true
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :spacing 10
                    :children (vec 
                                (concat
                                  [{:fx/type page-control-view
                                    :page page}]
                                  (for [[key area] regions]
                                    {:fx/type :v-box
                                     :spacing 0
                                     :children [{:fx/type :label
                                                 :text (str key)}
                                                {:fx/type page-area-view
                                                 :page page
                                                 :renderer renderer
                                                 :area area}
                                                {:fx/type :label
                                                 :text (get regions-text key)}]}
                                    )))}}}))


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
