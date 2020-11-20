(ns zen.doc.md
  (:import
   org.commonmark.parser.Parser
   java.util.Arrays
   org.commonmark.ext.gfm.tables.TablesExtension
   org.commonmark.renderer.html.HtmlRenderer))

(defn parse [s]
  (when s
    (let [exts (Arrays/asList (to-array [(TablesExtension/create)]))
          parser (-> (Parser/builder)
                     (.extensions exts)
                     .build)
          renderer (-> (HtmlRenderer/builder)
                       (.extensions exts)
                       .build)]
      (.render renderer (.parse parser s)))))

