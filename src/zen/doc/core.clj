(ns zen.doc.core
  (:require
   [zen.core :as zen]
   [ring.middleware.head]
   [ring.util.codec]
   [hiccup.core]
   [ring.util.response]
   [stylo.core :refer [c]]
   [route-map.core]
   [clojure.java.shell]
   [org.httpkit.server]
   [clojure.string :as str]))

(def h2 (c :border-b [:py 2] :font-bold))

(defn url [ctx & pth]
  (str (or (:base-url ctx) "") "/" (str/join "/" pth) ".html"))


(defn layout [{ztx :ztx :as ctx} & content]
  (hiccup.core/html
   [:html
    [:head
     [:meta {:charset "utf-8"}]
     [:link {:rel "stylesheet" :href "/zen.css"}]
     [:title "zen"]]
    [:body {:class (c [:p 0])}
     [:div {:class (c :flex [:space-x 6])}
      [:div {:class (c :border [:px 4] [:py 2] [:w 70]  :text-sm [:text :gray-700] [:bg :gray-100])}
       [:h2 {:class h2} "Tags"]
       [:div {:class (c [:pl 2])}
        (for [[ns _] (->> (:tags @ztx) (sort-by first))]
          [:div [:a {:href (url ctx "tags" ns)} (str ns)]])]

       [:h2 {:class h2} "Ns"]
       [:div {:class (c [:pl 2])}
        (for [[ns _] (->> (:ns @ztx) (sort-by first))]
          [:div [:a {:href (url ctx "ns" ns)} (str ns)]])]

       [:h2 {:class h2} "Symbols"]
       [:div {:class (c [:pl 2])}
        (for [[ns _] (->> (:symbols @ztx) (sort-by first))]
          [:div [:a {:href (url ctx "symbols" ns)} (str ns)]])]]
      [:div content]]]]))

(defn index [{ztx :ztx} req]
  [:div "index"])


(defn zen-css [_ _]
  {:status 200
   :body (stylo.core/compile-styles @stylo.core/styles)
   :headers {"content-type" "text/css"}})


(defn edn [ctx x]
  (cond
    (map? x) [:div {:class (c :flex)}
              [:div {:class (c [:text :gray-300] :bold [:mx 1])} "{"]
              [:div
               (for [[k v] x]
                 [:div {:class (c :flex [:ml 2])}
                  (edn ctx k) (edn ctx v)])]]
    (set? x) [:div {:class (c :flex)}
              [:div {:class (c [:text :gray-300] :bold [:mx 1])} "{"]
              (for [v x]
                [:div {:class (c :flex [:ml 2])} (edn ctx v)])]
    (sequential? x) [:div {:class (c :flex)}
                     [:div {:class (c [:text :gray-300] :bold [:mx 1])} "["]
                     [:div
                      (for [[idx v] (map-indexed (fn [i x] [i x]) x)]
                        [:div {:class (c :flex [:ml 2])}
                         [:div {:class (c [:text :gray-300] :bold [:mx 1])} (str "[" idx "]")]
                         (edn ctx v)])]]
    (number? x) [:div {:class (c [:text :orange-600])} x]
    (string? x) [:div {:class (c [:text :yellow-700])} (pr-str x)]
    (keyword? x) [:b {:class (c [:mr 2] :font-bold [:text :green-700])
                      :title (pr-str (meta x))}
                  (pr-str x)]
    (symbol? x)  [:a {:class (c [:text :blue-800] [:mr 2]) :href (url ctx "symbols" (str x))}  (pr-str x)]
    :else [:div "?" (pr-str x)]))


(defn ns-page [{ztx :ztx :as ctx} {{ns :ns} :route-params}]
  (let [sch (get-in @ztx [:ns (symbol ns)])]
    [:div
     [:div {:class (c :text-xl :font-bold :border-b [:mb 4]) } ns]
     (edn ctx sch)]))

(defn ns-index [ctx req]
  [:div "ns idx"]
  )

(defn tags-page [{ztx :ztx :as ctx} {{sym :sym*} :route-params}]
  (let [tag-nm (str/join "/" sym)
        tag-sym (symbol tag-nm)
        tag-sch (zen/get-symbol ztx tag-sym)
        tags (zen/get-tag ztx tag-sym)]
    [:div
     [:div {:class (c :text-xl :font-bold :border-b [:mb 4]) } tag-nm]
     [:h2 {:class h2} "Referred from:"]
     [:div
      (for [t (sort tags)]
        (let [desc (:zen/desc (zen/get-symbol ztx t))]
          [:div {:class (c [:space-y 1] [:mr 3])}
           (edn ctx t) (when desc [:span {:class (c [:text :gray-600])} (str ": " desc)])]))]
     [:h2 {:class h2} "Definition"]
     (edn ctx tag-sch)]))

(defn symbol-page [{ztx :ztx :as ctx} {{sym :sym*} :route-params}]
  (let [tag-nm (str/join "/" sym)
        tag-sch (zen/get-symbol ztx (symbol tag-nm))]
    [:div
     [:div {:class (c :text-xl :font-bold :border-b [:mb 4]) } tag-nm]
     (edn ctx tag-sch)]))

(defn tags-index [ctx req]
  [:div (pr-str req)])

(defn tags-gen [{ztx :ztx}]
  (keys (:tags @ztx)))

(defn ns-gen [{ztx :ztx}]
  (keys (:ns @ztx)))

(defn symbols-gen [{ztx :ztx}]
  (keys (:symbols @ztx)))

(def routes
  {:GET {:fn index :ext :html}
   "zen"     {:GET {:ext :css :fn zen-css}}
   "index"   {:GET {:ext :html :fn index}} 
   "tags"    {:GET {:ext :html :fn tags-index}
              [:sym*] {:gen tags-gen
                       :GET {:ext :html :fn tags-page}}}
   "ns"      {[:ns] {:gen ns-gen
                     :GET {:ext :html :fn ns-page}}}
   "symbols" {[:sym*] {:gen symbols-gen
                       :GET {:ext :html :fn symbol-page}}}})


(defn html [ctx content]
  {:body (layout ctx content)
   :headers {"content-type" "text/html"}
   :status 200})

(defn handle-static [{meth :request-method uri :uri :as req}]
  (when (and (#{:get :head} meth)
             (or (str/starts-with? (or uri "") "/static/")
                 (str/starts-with? (or uri "") "/favicon.ico")))
    (let [opts {:root "public"
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

(defn dispatch [ctx {uri :uri meth :request-method :as req}]
  (or (handle-static req)
      (let [[uri ext] (parse-uri uri)]
        (println ">>>" meth uri ext)
        (if-let [{{handler :fn rext :ext} :match params :params} (route-map.core/match [meth uri] routes)]
          (if (or (= rext ext) (and (set? rext) (contains? rext ext)))
            (let [resp (handler ctx (assoc req :route-params params :ext ext))]
              (if (= ext :html)
                (html ctx resp)
                resp))
            (html ctx [:div [:h1 "Not supported extension: " (pr-str ext) ", expected " (pr-str rext)]]))
          (html ctx [:div [:h1 "Not Found"] (edn ctx req)])))))

(defn start [ctx {port :port}]
  (let [srv (org.httpkit.server/run-server #(dispatch @ctx %) {:port (or port 4321)})]
    (swap! ctx assoc :web/server srv)
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

(defn build [ctx]
  (gen-route ctx [] {} routes))

(comment
  (do
    (defonce ctx (atom {}))

    (do (swap! ctx (fn [x] (assoc x :self ctx))) :ok)
    (do (swap! ctx assoc :ztx (zen.core/new-context)) :ok)
    (zen.core/read-ns (:ztx @ctx) 'zen.tests.schema)

    )

  (build @ctx)

  (:ztx @ctx)

  (start ctx {})


  )
