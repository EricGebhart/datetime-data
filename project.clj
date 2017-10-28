(defproject org.eag.datetime-data "0.1.0"
  :description "date time normalization functions for consistent date data"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [midje "1.8.3"]
                 [clj-time "0.14.0"]]

  :repositories {"local" ~(str (.toURI (java.io.File. "/home/eric/.m2/repository")))})
