(ns zen.doc.core
  (:require
   [zen.core :as zen]
   [ring.middleware.head]
   [ring.util.codec]
   [hiccup.core]
   [ring.util.response]
   [clojure.walk]
   [stylo.core :refer [c]]
   [route-map.core]
   [clojure.java.shell]
   [org.httpkit.server]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [zen.doc.md])
  (:gen-class))

(def h2 (c :border-b [:py 2] :font-bold))

(defn cls [& xs]
  (->> xs (filter identity) (map name) (str/join " ")))

(defn url [ctx pth & [ext]]
  (str (or (:base-url @ctx) "") "/" (str/join "/" pth) (when ext (str "." (name ext)))))

(defn symbol-url [ctx sym]
  (url ctx ["symbols" sym] :html))

(defn edn [ctx x]
  (cond
    (map? x) [:div {:class (c :flex)}
              [:div {:class (c [:text :gray-500] :bold [:mx 1])} "{"]
              [:div
               (for [[k v] x]
                 [:div {:class (c :flex [:ml 2])}
                  (edn ctx k) (edn ctx v)])]]
    (set? x) [:div {:class (c :flex)}
              [:div {:class (c [:text :gray-500] :bold [:mx 1])} "#{"]
              (for [v x]
                [:div {:class (c :flex [:ml 2])} (edn ctx v)])]
    (sequential? x) [:div {:class (c :flex)}
                     [:div {:class (c [:text :gray-500] :bold [:mx 1])} "["]
                     (if (or (keyword? (first x))
                             (symbol? (first x)))
                       [:div {:class (c [:text :green-700] :bold [:mx 1])} (str  (str/join " " x) "]")]
                       [:div
                        (for [[idx v] (map-indexed (fn [i x] [i x]) x)]
                          [:div {:class (c :flex [:ml 2])}
                           [:div {:class (c [:text :gray-500] :bold [:mx 1])} (str "[" idx "]")]
                           (edn ctx v)])])]
    (number? x) [:div {:class (c [:text :orange-600])} x]
    (string? x) [:div {:class (c [:text :yellow-700])} (pr-str x)]
    (keyword? x) [:b {:class (c [:mr 2] :font-bold [:text :green-700])
                      :title (pr-str (meta x))}
                  (pr-str x)]
    (symbol? x)  [:a {:class (c [:text :blue-800] [:mr 2]) :href (url ctx ["symbols" (str x)] :html)}  (pr-str x)]
    :else [:div "?" (pr-str x)]))


(defn symbol-icon [ctx v]
  (let [tgs (:tags v)]
    [:div {:class
           (str/join " "
                     (mapv name
                           [(c [:w 3] [:h 3] [:mr 1]
                               {:border-radius "100%" :font-size "9px" :text-align "center" :line-height "0.75rem"})
                            (cond
                              (contains? tgs 'zen/type)     (c  [:bg :green-400])
                              (contains? tgs 'zen/tag)      (c  [:bg :orange-300])
                              (contains? tgs 'zen/property) (c  [:bg :blue-300])
                              (contains? tgs 'zen/valueset) (c  [:bg :pink-300])
                              (contains? tgs 'zen/schema)   (c  [:bg :green-300])
                              :else                         (c :border [:bg :gray-300]))]))
           :title (str/join " " tgs)}
     (cond
         (contains? tgs 'zen/tag) "#"
         (contains? tgs 'zen/type)  "T"
         (contains? tgs 'zen/valueset)  "V"
         (contains? tgs 'zen/schema) "S")]))

(defn render-tree [ctx syms]
  [:div {:class (c [:pl 4])}
   (for [[k v] (sort-by first syms)]
     [:a {:href (when-let [nm (:name v)] (symbol-url ctx nm))
          :class (c :block :flex :items-baseline :align-baseline [:py 0.25] [:text :gray-700]
                    [:hover [:text :gray-900]])}
      (when-let [tgs (:tags v)]
        (symbol-icon ctx v))
      [:div (str k)]
      (when-let [ch (:children v)]
        [:span "/"
         (render-tree ctx ch)])])])

(defn left-sidebar [ctx]
  (let [syms (->>
              (:symbols @ctx)
              (sort-by first)
              (reduce (fn [acc [nm data]]
                        (let [pth (interpose :children (str/split (str nm) #"[./]"))]
                          (assoc-in acc pth {:name nm :path pth :tags (:zen/tags data) :desc (:zen/desc data)})))
                      {}))]

    [:div {:class (c [:px 4] [:py 2] [:w 80] [:w-min 80]
                     :text-sm [:text :gray-700] [:bg :gray-100])}
     [:div {:class (c [:pl 2])}
      (render-tree ctx syms)]]))

(def top-menu-link (c [:text :red-600]))

(defn top-menu [ctx]
  (let [main (:main @ctx)]
    [:div {:class (c :flex :border-b [:px 0] [:py 4] {:box-shadow "0 3px 8px 0 rgba(116, 129, 141, 0.1)"})}
     [:a {:class (c [:w 80] :block :text-center :text-xl {:border-right "1px solid #ddd"})
          :href (url ctx ["index"] :html)}
      (when-let [logo (:logo main)]
        [:img {:class (c :inline-block  [:pr 2] {:height "20px"})
               :src (url ctx ["static" logo])}])
      (or (:title main) "Title")]
     [:div {:class (c :flex [:space-x 4] [:px 12])}
      [:a {:class top-menu-link :href (url ctx ["tags"] :html)} "Tags"]
      [:a {:class top-menu-link :href (url ctx ["symbols"] :html)} "Symbols"]
      [:a {:class top-menu-link :href (url ctx ["ns"] :html)} "Namespaces"]]]))

(defn layout [ctx & content]
  (let [main (:main @ctx)]
    (hiccup.core/html
     [:html
      [:head
       [:meta {:charset "utf-8"}]
       [:link {:rel "stylesheet" :href "/zen.css"}]
       [:title (:title main)]]
      [:body {:class (c [:p 0] [:m 0])}
       (top-menu ctx)
       [:div {:class (c :flex [:space-x 6])}
        (left-sidebar ctx)
        [:div {:class (c [:p 4] :flex-1)} content]]]])))

(defmulti render-page (fn [_ {tp :type}] tp))

(defmethod render-page 'zen.doc/md
  [_ {f :file}]
  (when-let [res (io/resource f)]
    (zen.doc.md/parse (slurp res))))

(defmethod render-page 'zen.doc/html
  [_ {f :file}]
  [:pre
   (slurp (io/resource f))])

(defn index [ctx req]
  (let [main (:main @ctx)
        page (when-let [ip (:index-page main)]
                (zen/get-symbol ctx ip))]
    (render-page ctx page)))

(defn zen-css [_ _]
  {:status 200
   :body (stylo.core/compile-styles @stylo.core/styles)
   :headers {"content-type" "text/css"}})


(defn ns-page [ctx {{ns :ns} :route-params}]
  (let [sch (get-in @ctx [:ns (symbol ns)])]
    [:div
     [:div {:class (c :text-xl :font-bold :border-b [:mb 4]) } ns]
     (edn ctx sch)]))

(defn ns-index [ctx req]
  [:div "ns idx"])

(defn tags-page [ctx {{sym :sym*} :route-params}]
  (let [tag-nm (str/join "/" sym)
        tag-sym (symbol tag-nm)
        tag-sch (zen/get-symbol ctx tag-sym)
        tags (zen/get-tag ctx tag-sym)]
    [:div
     [:div {:class (c :text-xl :font-bold :border-b [:mb 4]) } tag-nm]
     [:h2 {:class h2} "Referred from:"]
     [:div
      (for [t (sort tags)]
        (let [desc (:zen/desc (zen/get-symbol ctx t))]
          [:div {:class (c [:space-y 1] [:mr 3])}
           (edn ctx t) (when desc [:span {:class (c [:text :gray-600])} (str ": " desc)])]))]
     [:h2 {:class h2} "Definition"]
     (edn ctx tag-sch)]))

(declare render-schema)

(defn type-icon [ctx tp]
  [:div {:class
         (str/join " "
                   (mapv name
                         [(c [:w 4] [:h 4] [:mr 1]
                             {:border-radius "100%" :font-size "10px" :text-align "center" :line-height "1rem"})
                          (cond
                            (= tp 'zen/set) (c [:bg :blue-300])
                            (= tp 'zen/map) (c [:bg :green-300])
                            (= tp 'zen/case) (c [:bg :red-500])
                            (= tp 'zen/string) (c [:bg :orange-300])
                            (= tp 'zen/datetime) (c [:bg :orange-300])
                            (= tp 'zen/date) (c [:bg :orange-300])
                            (= tp 'zen/boolean) (c [:bg :orange-300])
                            (= tp 'zen/keyword) (c [:bg :orange-300])
                            (= tp 'zen/number) (c [:bg :orange-300])
                            :else  (c [:bg :gray-300]))]))}
   (cond
     (= tp 'zen/set) "#"
     (= tp 'zen/map) "{}"
     (= tp 'zen/case) "?"
     :else (first (last (str/split (str tp) #"/"))))])

(defn render-zen-map [ctx sch]
  [:div 
   (when-let [cfs (:confirms sch)]
     [:div "map: "
      (for [cf cfs]
        [:a {:href (symbol-url ctx cf) :class (c [:text :green-700])}
         (str cf)])
      ""])
   (for [[k v] (->> (:keys sch)
                    (sort-by (fn [[_ v]] (:row (meta v)))))]
     [:div
      [:div {:class (c :flex [:space-x 2] :items-center)}
       (type-icon ctx (:type v))
       [:b {:class (c {:font-weight "500"})}
        (if (keyword? k)
          (subs (str k) 1)
          (str k))
        (when (contains? (:require sch) k)
          [:span {:class (c [:ml 1] [:text :red-700])} "*"])]
       (when-let [tp (:type v)]
         [:a {:href (symbol-url ctx tp) :class (c [:text :blue-700])}
          (str tp)])
       (when-let [cfs (:confirms v)]
         [:div (for [cf cfs]
            [:a {:href (symbol-url ctx cf) :class (c [:text :green-700])}
             (str cf)])])]
      (when-let [desc (:zen/desc v)]
        [:div {:class (c :text-xs [:text :gray-600] [:ml 7])}
         (subs desc 0 (min (count desc) 100))])
      (when (not (empty? (dissoc v :confirms :zen/desc)))
        [:div {:class (c [:pl 8])}
         (render-schema ctx (dissoc v :confirms :zen/desc))])])])

(defn render-zen-set [ctx sch]
  #_(when-let [evr (:every sch)]
    [:div [:b "every =>"]
     (render-schema ctx evr)]))

(defn render-zen-vector [ctx sch]
  (when-let [evr (:every sch)]
    [:div [:b "vector "
           (when-let [tp (:type evr)] [:a {:href (symbol-url ctx tp) :class (c [:text :blue-700])} (str tp)])
           (when-let [cfs (:confirms evr)]
             (for [cf cfs]
               [:a {:href (symbol-url ctx cf) :class (c [:text :green-700])}
                (str cf)]))

           " : "]
     [:div {:class (c [:ml 4])}
      (render-schema ctx evr)]]))

(defn render-zen-case [ctx sch]
  #_(when-let [cs (:case sch)]
    [:div
     (for [{w :when th :then} cs]
       [:div
        [:div {:class (c :flex)} [:b "when"] (render-schema ctx w)] 
        (when th
          [:div {:class (c [:ml 2])} [:b "then"] (render-schema ctx th)])])]))

(defn render-schema [ctx sch]
  (cond
    (= 'zen/map (:type sch)) (render-zen-map ctx sch)
    (= 'zen/set (:type sch)) (render-zen-set ctx sch)
    (= 'zen/vector (:type sch)) (render-zen-vector ctx sch)
    (= 'zen/case (:type sch)) (render-zen-case ctx sch)
    (= 'zen/keyword (:type sch)) ""
    (= 'zen/string (:type sch)) ""
    (= 'zen/integer (:type sch)) ""
    (= 'zen/number (:type sch)) ""
    (= 'zen/boolean (:type sch)) ""
    (= 'zen/datetime (:type sch)) ""
    (= 'zen/date (:type sch)) ""
    (nil? (:type sch)) ""
    (= 'zen/symbol (:type sch)) [:div {:class (c :flex [:space-x 1])}
                                 [:a {:href (symbol-url ctx 'zen/symbol)
                                      :class (c [:text :green-700])}
                                  "zen/symbol"]
                                 (when-let [tgs (:tags sch)]
                                   [:div {:class (c :flex [:space-x 1])}
                                    [:div "#{"]
                                    (for [t tgs]
                                      [:a {:class (c [:mr 2] :class (c [:text :blue-700]))
                                           :href (symbol-url ctx t)}
                                       (str t)])
                                    [:div "}"]])]
    (= 'zen/any (:type sch)) ""
    :else (edn ctx sch)))

(defn render-valueset [ctx {vs :values}]
  (let [cols (sort (keys (first vs)))]
    [:table
     [:thead (for [col cols]
               [:th {:class (c [:bg :gray-100] [:p 1] [:pr 4] :border-b :whitespace-no-wrap {:text-align "left" :font-weight "500"})}
                (name col)])]
     [:tbody
      (for [v vs]
        [:tr 
         (for [col cols]
           [:td {:class (c [:pl 0] [:pr 4] [:pt 1.5] [:pb 1] :border-b)}
            (get v col)])])]]))

(defn resolve-views [ctx tags]
  (-> 
   (->> (zen/get-tag ctx 'zen.doc/tag-view)
        (mapv (fn [tv] (zen/get-symbol ctx tv)))
        (reduce (fn [acc x]
                  (if (contains? tags (:tag x))
                    (conj acc x)
                    acc))
                []))
   (conj {:zen/name 'zen.doc/view-for-edn
          :title "edn"
          :slag "edn"})))

(defmulti tag-view (fn [ctx view model] (:zen/name view)))

(defmethod tag-view
  'zen.doc/view-for-schema
  [ctx view model]
  (render-schema ctx model))

(defmethod tag-view
  'zen.doc/view-for-valueset
  [ctx view model]
  (render-valueset ctx model))

(defmethod tag-view
  'zen.doc/view-for-edn
  [ctx view model]
  (edn ctx model))


(defmethod tag-view
  'zen.doc/view-for-tag
  [ctx view model]
  (let [tag-nm (:zen/name model)
        tags (zen/get-tag ctx tag-nm)]
    [:div {:class (c [:p 0])}
     (for [t (sort tags)]
       (let [desc (:zen/desc (zen/get-symbol ctx t))]
         [:div {:class (c [:space-y 1] :border-b [:py 1])}
          (edn ctx t) (when desc [:span {:class (c [:text :gray-600])}
                                  desc])]))]))


(defn symbol-page [ctx {{sym :sym*} :route-params params :params :as req}]
  (let [nm (str/join "/" sym)
        model (zen/get-symbol ctx (symbol nm))
        views (resolve-views ctx (:zen/tags model))
        cur-view (if-let [v (:view params)]
                   (->> views (filter (fn [x] (= (:slag x) v))) first)
                   (first views))]

    [:div
     [:div {:class (c :text-2xl :font-bold :border-b [:mb 4]
                      :items-baseline
                      :flex [:space-x 3]
                      {:vertical-align "baseline"})}
      [:div nm]
      [:div
       (for [t (:zen/tags model)]
         [:a {:class (c [:mr 2] :text-xl [:text :gray-500] {:font-weight "400" :vertical-align "baseline"})
              :href (symbol-url ctx t)}
          "#" (str t)])]]

     [:p {:class (c [:text :gray-700])}(:zen/desc model)]
     [:br]

     [:div {:class (c :flex :w-full [:mt 2])}
      [:div {:class (c [:w 8] :border-b)}]
      (for [v views]
        [:a {:class (cls
                     (c :flex-1 [:py 1] :border :text-center [:bg :gray-200] [:text :gray-600])
                     (when (= cur-view v)
                       (c [:text :gray-900]  {:border-bottom-color "transparent" :background-color "white"})))
             :href (str "?view=" (:slag v))}
         (:title v)])
      [:div {:class (c [:w 8] :border-b)}]]
     [:div {:class (c :border [:px 10] [:py 8] {:border-top "none"})}
      (tag-view ctx cur-view model)]]))

(def h1 (c :text-xl))

(def block-link (c :inline-block [:m 2] [:p 2]
                   :border
                   :rounded
                   :float [:w-min 46]
                   [:text :gray-600]
                   [:hover [:bg :gray-100] [:text :gray-700]]))

(defn tags-index [ctx req]
  [:div
   [:h1 {:class h1} "Tags"]
   [:div 
    (for [[ns _] (->> (:tags @ctx) (sort-by first))]
      [:a {:href (url ctx ["tags" ns] :html)
           :class block-link} (str ns)])]])



(defn tags-gen [ctx]
  (keys (:tags @ctx)))

(defn ns-gen [ctx]
  (keys (:ns @ctx)))

(defn symbols-gen [ctx]
  (keys (:symbols @ctx)))

(defn symbols-index [ctx req]
  [:div
   [:h1 {:class h1} "Symbols"]
   [:div
    (for [[ns _] (->> (:symbols @ctx) (sort-by first))]
      [:a {:href (url ctx ["symbols" ns] :html)
           :class block-link }
       (str ns)])]])

(def routes
  {:GET {:fn index :ext :html}
   "zen"     {:GET {:ext :css :fn zen-css}}
   "index"   {:GET {:ext :html :fn index}} 
   "tags"    {:GET {:ext :html :fn tags-index}
              [:sym*] {:gen tags-gen
                       :GET {:ext :html :fn tags-page}}}
   "ns"      {[:ns] {:gen ns-gen
                     :GET {:ext :html :fn ns-page}}}
   "symbols" {:GET {:ext :html :fn symbols-index}
              [:sym*] {:gen symbols-gen
                       :GET {:ext :html :fn symbol-page}}}})

(defn html [ctx content]
  {:body (layout ctx content)
   :headers {"content-type" "text/html"}
   :status 200})

(defn handle-static [{meth :request-method uri :uri :as req}]
  (when (and (#{:get :head} meth)
             (or (str/starts-with? (or uri "") "/static/")
                 (str/starts-with? (or uri "") "/favicon.ico")))
    (let [opts {:root "static"
                :index-files? true
                :allow-symlinks? true}
          path (subs (ring.util.codec/url-decode (:uri req)) 8)]
      (-> (ring.util.response/resource-response path opts)
          (ring.middleware.head/head-response req)))))

(defn parse-uri [uri]
  (let [ext (when-let [item (last (str/split uri #"/"))]
              (last (str/split item  #"\.")))]
    (println "EXT" ext)
    (if ext
      [(subs uri 0 (- (count uri) (inc (count ext)))) (keyword ext)]
      [uri :html])))

(defn form-decode [s]
  (when s
    (clojure.walk/keywordize-keys (ring.util.codec/form-decode s))))

(defn dispatch [ctx {uri :uri meth :request-method :as req}]
  (or (handle-static req)
      (let [[uri ext] (parse-uri uri)]
        (println ">>>" meth uri ext)
        (let [params (form-decode (:query-string req))]
          (if-let [{{handler :fn rext :ext} :match route-params :params} (route-map.core/match [meth uri] routes)]
            (if (or (= rext ext) (and (set? rext) (contains? rext ext)))
              (let [resp (handler ctx (assoc req :route-params route-params :ext ext :params params))]
                (if (= ext :html)
                  (html ctx resp)
                  resp))
              (html ctx [:div [:h1 "Not supported extension: " (pr-str ext) ", expected " (pr-str rext)]]))
            (html ctx [:div [:h1 "Not Found"] (edn ctx req)]))))))

(defn start [ctx {port :port nss :ns main-ns :main}]
  (doseq [n nss] (zen/read-ns ctx n))
  (let [main (zen/get-symbol ctx main-ns)
        srv (org.httpkit.server/run-server #(dispatch ctx %) {:port (or port 4321)})]
    (swap! ctx assoc :web/server srv :main main)
    :ok))

(defn gen-route [ctx pth params route]
  (if (map? route)
    (doseq [[k v] route]
      (cond
        (= :GET k)
        (when-not (empty? pth)
          (let [ext (:ext v)
                uri (str (str/join "/" pth) "." (name ext))
                body (-> (dispatch ctx {:uri uri :request-method :get :route-params params}) :body)
                fpth (str "build/" uri)
                dpth (str/join "/" (butlast (str/split fpth #"/")))]
            (when-not (empty? pth)
              (clojure.java.shell/sh "bash" "-c" (format "mkdir -p %s" dpth)))
            (println "*" fpth)
            (spit fpth body)))
        (and (vector? k) (keyword? (first k)))
        (if-let [gen (:gen v)]
          (let [param (first k)]
            (doseq [it (gen ctx)]
              (gen-route ctx (conj pth it) (assoc params param it) v)))
          (println "SKIP:" k v))
        (string? k) (gen-route ctx (conj pth k) params v)))))

(defn build [{base-url :base-url ns :ns :as ctx}]
  (gen-route ctx [] {} routes))


(defn -main [& args]
  (println "Hello"))

(comment
  (defonce ctx (zen/new-context))

  (build @ctx)

  (start ctx {:ns #{'zen 'zen-doc} :main 'zen-doc/index})
  (zen/read-ns ctx 'zen-doc)
  (zen/read-ns ctx 'zen.doc)

  (zen/get-tag ctx 'zen.doc/tag-view)

  (keys @ctx)


  )
