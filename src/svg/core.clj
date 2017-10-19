(ns svg.core
    (:require
      [compojure.core :refer :all]
      [compojure.route :as route]
      [clj-http.client :as client]
      [clojure.xml :as xml]
      [clojure.java.io :as io]
      [clojure.zip :as zip]
      [ring.middleware.defaults :refer [wrap-defaults site-defaults]])
    (:use ring.adapter.jetty))

(def conf {:site  ""  ; https://you.myjetbrains.com
           :url   ""  ; https://you.myjetbrains.com/issue/
           :rest  ""  ; https://you.myjetbrains.com/rest/issue/
           :login ""  ; email
           :pwd   ""  ; pwd
           })

(def sess (atom []))


(defn login [name pwd]
      (let [response (client/post (str (:site conf) "/rest/user/login")
                                  {:content-type :application/x-www-form-urlencoded
                                   :body (str "login=" name "&password=" pwd)})]
        (println :cookies response)
        (swap! sess :cookies response)))


(defn parse [s]
      (xml/parse
        (java.io.ByteArrayInputStream. (.getBytes s))))


(defn fetch-page [url]
      (:body (client/get url {:cookies @sess})))


(defn get-by [xml type name]
      (for [x (xml-seq xml)
            :when (= (get-in x [:attrs type]) name)]
        (first (:content x))))


(defn get-name [data]
      (for [x (get-by data :name "summary")]
        (first (:content x))))


(defn parse-subs [sub]
      (let [tm (transient [])]
      (doseq [s sub]
             (let [resp (parse (fetch-page (str (:rest conf) s)))
                   issue s]
                  (conj! tm {:issue issue :name (first (get-name resp))})))
           (persistent! tm)))

(defn trunc
      [s n]
      (subs s 0 (min (count s) n)))


(defn show-row [data]
      (let [tm (transient [])
            y (atom 0)]
        (doseq [d data]
          (swap! y (partial + 30))
          (conj! tm (str "
          <text x='30' y='" (str @y) "' font-size='16px' text-anchor='left'>
               <a xlink:href='"(:url conf)(:issue d)"' target='_blank' xlink:title='" (:name d) "'><tspan x='10'>" (:issue d) "</tspan></a>
               <tspan x='100'>" (trunc (:name d) 50) "</tspan>
               </text>
               "))
          )
        (clojure.string/join (persistent! tm))))


(defn show-all [data]
      (str "<?xml version='1.0' standalone='no'?>\n<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'\n'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\n<svg width='100%' height='100%' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>\n\n<g id='rowGroup' transform='translate(0, 0)'>"
           "<defs>\n    <style type=\"text/css\"><![CDATA[\n      a {\n        fill: darkblue;\n        text-decoration: underline;\n      }\n      a:hover {\n        fill: blue;\n      }\n      a circle, a rect {\n        fill: lightblue;\n        stroke-width: 2px;\n        stroke: green;\n      }\n      \n      a:hover circle, a:hover rect {\n        fill: white;\n        stroke: lightgreen;\n      }\n      \n    ]]></style>\n\t</defs>"
           (show-row data)
           "</g>\n</svg>"
           ))


(defn try-render [issue]
      (let [page (parse (fetch-page (str (:rest conf) issue)))
            sub-tasks  (get-by page :type "Subtask")
            sub-data (parse-subs sub-tasks)]

        {:status  200
         :headers {"Content-Type" "image/svg+xml"}
         :body (show-all sub-data)}))

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