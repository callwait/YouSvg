(ns svg.core
  (:require
    [compojure.core :refer :all]
    [compojure.route :as route]
    [clj-http.client :as client]
    [clojure.xml :as xml]
    [clojure.java.io :as io]
    [clojure.zip :as zip]
    [clojure.string :as str]
    [clojure.edn :as edn]
    [ring.middleware.defaults :refer [wrap-defaults site-defaults]])
  (:use ring.adapter.jetty))


(defn load-config
      "Given a filename, load & return a config file"
      [filename]
      (edn/read-string (slurp filename)))

(def conf (load-config "config.edn"))

(def sess (atom []))


(defn login [name pwd]
      "function auth by login & password; return cookies"
      (let [response (client/post (str (:site conf) "/rest/user/login")
                                  {:content-type :application/x-www-form-urlencoded
                                   :body         (str "login=" name "&password=" pwd)})]
        (println :cookies response)
        (swap! sess :cookies response)))


(defn parse [s]
      (xml/parse
        (java.io.ByteArrayInputStream. (.getBytes s))))


(defn fetch-page [url]
      "download page"
      (:body (client/get url {:cookies @sess})))


(defn get-by [xml type name]
      (for [x (xml-seq xml)
            :when (= (get-in x [:attrs type]) name)
            :when (not= (get-in x [:attrs :role]) "subtask of")
            ]
        (first (:content x))))




(defn get-name [data]
      (for [x (get-by data :name "summary")]
        (first (:content x))))

(defn get-sp [data]
      (for [x (get-by data :name "Story Points")]
        (first (:content x))))

(defn get-state [data]
      (for [x (get-by data :name "State")]
        (first (:content x))))

(defn get-app [data]
      (for [x (get-by data :name "Related Apps")]
        (first (:content x))))


(defmacro dopar [seq-expr & body]
  "macro for parallel request"
  (assert (= 2 (count seq-expr)) "single pair of forms in sequence expression")
  (let [[k v] seq-expr]
    `(apply await
            (for [k# ~v]
              (let [a# (agent k#)]
                (send a# (fn [~k] ~@body))
                a#)))))


(defn parse-subs [sub]
      "Get and parse issues list"
      (let [tm (transient [])]
        (dopar [s sub]
          (let [resp (parse (fetch-page (str (:rest conf) s)))
                issue s]
            (conj! tm {:issue issue
                       :name  (first (get-name resp))
                       :state (first (get-state resp))
                       :sp    (first (get-sp resp))
                       :app   (first (get-app resp))
                       })))
        (persistent! tm)))


(defn trunc
      [s n]
      (subs s 0 (min (count s) n)))


(defn show-row [data]
      (let [tm (transient [])
            y (atom 30)
            c (atom 0)]
        (doseq [d data]
          (swap! y (partial + 30))
          (swap! c (partial + 1))
          (conj! tm (str
               (if (even? @c)
                 (str "<rect x='5' y='" (str (- @y 18)) "' width='800' height='28' fill='gainsboro'/>"))

               "<text x='30' y='" (str @y) "' font-size='16px' text-anchor='left'>
               <a xlink:href='" (:url conf) (:issue d) "' target='_blank' xlink:title='" (:name d) "'>
               <tspan x='10'>" (:issue d) "</tspan></a>
               <tspan x='90'>" (trunc (:app d) 16) "</tspan>
               <tspan x='220'>" (trunc (:name d) 52) "</tspan>
               <tspan x='650' " (if (= (:state d) "To Merge") (str "fill='green'")) ">" (:state d) "</tspan>
               <tspan x='750'>" (:sp d) "</tspan>
               </text>
               "))
          )
        (clojure.string/join (persistent! tm))))


(defn show-all [data]
      (str "<?xml version='1.0' standalone='no'?>\n<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'\n'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\n<svg width='100%' height='100%' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>\n\n<g id='rowGroup' transform='translate(0, 0)'>"
           "<defs>\n    <style type=\"text/css\"><![CDATA[\n      a {\n        fill: darkblue;\n        text-decoration: underline;\n      }\n      a:hover {\n        fill: blue;\n      }\n      a circle, a rect {\n        fill: lightblue;\n        stroke-width: 2px;\n        stroke: green;\n      }\n      \n      a:hover circle, a:hover rect {\n        fill: white;\n        stroke: lightgreen;\n      }\n      \n    ]]></style>\n\t</defs>"

           "<rect x='5' y='13' width='800' height='27' fill='gainsboro'/>
            <text x='30' y='30' font-size='15px' font-weight='bold' fill='brown' text-anchor='left'>
            <tspan x='10'>ID</tspan>
            <tspan x='90'>App</tspan>
            <tspan x='220'>Summary</tspan>
            <tspan x='650'>State</tspan>
            <tspan x='750'>SP</tspan>
            </text>"

           (show-row data)
           "</g>\n</svg>"
           ))


(defn try-render [issue]
      (let [page (parse (fetch-page (str (:rest conf) issue)))
            sub-tasks (get-by page :type "Subtask")
            sub-data (time (parse-subs sub-tasks))]

        {:status  200
         :headers {"Content-Type" "image/svg+xml"}
         :body    (show-all sub-data)}))

(defn try-login [issue]
      (login (:login conf) (:pwd conf))
      (login (:login conf) (:pwd conf))
      (try-render issue)
      )

(defn make-svg [issue]
      (if (empty? @sess)
        (try-login issue)
        (try-render issue)))


(defroutes app-routes
           (GET "/" [] "Youtrack issue->svg mini service")
           (GET "/:id.svg" [id]
             (make-svg id))
           (route/not-found "Not Found"))

(defn -main []
      (run-jetty app-routes {:port 3000}))