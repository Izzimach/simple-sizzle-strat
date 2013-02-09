(defproject simplestrat "0.1.0-SNAPSHOT"
  :description "Simple strategy game"
  :url "http://interstellarechidna.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repl-options { :port 9000 }

  :plugins [[lein-cljsbuild "0.3.0"]
            [lein-ring "0.8.2"]]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [ring/ring-core "1.1.8"]
                 [ring/ring-jetty-adapter "1.1.8"]
                 [compojure "1.1.5"]]

  :profiles {:dev {:dependencies [[ring/ring-devel "1.1.8"]]}}

  :ring { :handler simplestrat.httpserver/app :port 5000}
  
  ;; only ring uses the clojure source path ATM
  :source-paths ["src-server"]
  
  :cljsbuild
  {
   :builds
   {
    ;; browser build target
    :main {
           :source-paths ["src-browser"]
           :compiler {
                      :output-to "resources/public/js/simplestrat.js"
                      :optimizations :simple
                      :pretty-print true
;;                      :libs ["closure/library/third_party/closure"]
;;                      :externs ["externs/createjs-externs.js"]
                      }
           }
    ;; node.js server script; not used, currently
    ;; we use the ring server
    :nodejs {
             :source-paths ["src-nodejs"]
             :compiler {
                        :output-to "server.js"
                        :optimizations :simple
                        :target :nodejs
                        }
             }
    }
   }
  )

