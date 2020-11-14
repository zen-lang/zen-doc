(ns zen.doc.core
  (:require
   [zen.core :as zen]
   [ring.middleware.head]
   [ring.util.codec]
   [hiccup.core]
   [ring.util.response]
   [stylo.core :refer [c]]
   [route-map.core]
   [org.httpkit.server]
   [clojure.string :as str]))

(def h2 (c :border-b [:py 2] :font-bold))

(defn url [& pth]
  (str "/" (str/join "/" pth)))


(defn layout [{ztx :ztx} & content]
  (hiccup.core/html
   [:html
    [:head
     [:meta {:charset "utf-8"}]
     [:style (stylo.core/compile-styles @stylo.core/styles)]
     [:title "zen"]]
    [:body {:class (c [:p 0])}
     [:div {:class (c :flex [:space-x 6])}
      [:div {:class (c :border [:px 4] [:py 2] [:w 70]  :text-sm [:text :gray-700] [:bg :gray-100])}
       [:h2 {:class h2} "Tags"]
       [:div {:class (c [:pl 2])}
        (for [[ns _] (->> (:tags @ztx) (sort-by first))]
          [:div [:a {:href (url "tags" ns)} (str ns)]])]

       [:h2 {:class h2} "Ns"]
       [:div {:class (c [:pl 2])}
        (for [[ns _] (->> (:ns @ztx) (sort-by first))]
          [:div [:a {:href (url "ns" ns)} (str ns)]])]

       [:h2 {:class h2} "Symbols"]
       [:div {:class (c [:pl 2])}
        (for [[ns _] (->> (:symbols @ztx) (sort-by first))]
          [:div [:a {:href (url "symbols" ns)} (str ns)]])]]
      [:div content]]]]))

(defn index [{ztx :ztx} req]
  [:div "index"])


(defn edn [x]
  (cond
    (map? x) [:div {:class (c :flex)}
              [:div {:class (c [:text :gray-300] :bold [:mx 1])} "{"]
              [:div
               (for [[k v] x]
                 [:div {:class (c :flex [:ml 2])}
                  (edn k) (edn v)])]]
    (set? x) [:div {:class (c :flex)}
              [:div {:class (c [:text :gray-300] :bold [:mx 1])} "{"]
              (for [v x]
                [:div {:class (c :flex [:ml 2])} (edn v)])]
    (sequential? x) [:div {:class (c :flex)}
                     [:div {:class (c [:text :gray-300] :bold [:mx 1])} "["]
                     [:div
                      (for [[idx v] (map-indexed (fn [i x] [i x]) x)]
                        [:div {:class (c :flex [:ml 2])}
                         [:div {:class (c [:text :gray-300] :bold [:mx 1])} (str "[" idx "]")]
                         (edn v)])]]
    (number? x) [:div {:class (c [:text :orange-600])} x]
    (string? x) [:div {:class (c [:text :yellow-700])} (pr-str x)]
    (keyword? x) [:b {:class (c [:mr 2] :font-bold [:text :green-700])
                      :title (pr-str (meta x))}
                  (pr-str x)]
    (symbol? x)  [:a {:class (c [:text :blue-800] [:mr 2]) :href (url "symbols" (str x))}  (pr-str x)]
    :else [:div "?" (pr-str x)]))


(defn ns-page [{ztx :ztx} {{ns :ns} :route-params}]
  (let [sch (get-in @ztx [:ns (symbol ns)])]
    [:div
     [:div {:class (c :text-xl :font-bold :border-b [:mb 4]) } ns]
     (edn sch)]))

(defn ns-index [ctx req]
  [:div "ns idx"]
  )

(defn tags-page [{ztx :ztx} {{sym :sym*} :route-params}]
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
           (edn t) (when desc [:span {:class (c [:text :gray-600])} (str ": " desc)])]))]
     [:h2 {:class h2} "Definition"]
     (edn tag-sch)]))

(defn symbol-page [{ztx :ztx} {{sym :sym*} :route-params}]
  (let [tag-nm (str/join "/" sym)
        tag-sch (zen/get-symbol ztx (symbol tag-nm))]
    [:div
     [:div {:class (c :text-xl :font-bold :border-b [:mb 4]) } tag-nm]
     (edn tag-sch)]))

(defn tags-index [ctx req]
  [:div (pr-str req)]
  )

(def routes
  {:GET index
   "tags"    {:GET tags-index
              [:sym*] {:GET tags-page}}
   "ns"      {[:ns] {:GET ns-page}}
   "symbols" {[:sym*] {:GET symbol-page}}})


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


(defn dispatch [{ztx :ztx :as ctx} {uri :uri meth :request-method :as req}]
  (or (handle-static req)
      (html ctx (if-let [{handler :match params :params} (route-map.core/match [meth uri] routes)]
                  (handler ctx (assoc req :route-params params))
                  {:status 404 :body (layout [:h1 "Not Found"])}))
      ))

(defn start [ctx {port :port}]
  (let [srv (org.httpkit.server/run-server #(dispatch @ctx %) {:port (or port 4321)})]
    (swap! ctx assoc :web/server srv)
    :ok))


(comment
  (do
    (defonce ctx (atom {}))

    (do (swap! ctx (fn [x] (assoc x :self ctx))) :ok)
    (do (swap! ctx assoc :ztx (zen.core/new-context)) :ok)
    (zen.core/read-ns (:ztx @ctx) 'zen.tests.schema)

    )


  (:ztx @ctx)

  (start ctx {})


  )
