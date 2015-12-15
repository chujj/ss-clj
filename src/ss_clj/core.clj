(ns ss-clj.core
  (:gen-class)
  (:use (ss-clj utils))
  (:require [ss-clj.socks5_server :as s5server]
            [ss-clj.local_server :as lserver]
            [ss-clj.remote_server :as rserver])
  (:import (java.net ServerSocket Socket InetSocketAddress UnknownHostException)
           (java.io BufferedReader InputStreamReader InputStream ByteArrayInputStream PrintWriter)))

(defn println-socket
  [socket]
  (let [buff (byte-array 8)
        is (.getInputStream socket)]
    (while (let [readcount (.read is buff 0 8)
                 continue? (> readcount 0)]
             (if continue?
               (println "println-socket---" (seq buff)))
             continue?))))

(def socks5-proxy-server s5server/handle-client-request)
(def local-server lserver/handle-client-request)
(def remote-server rserver/handle-client-request)

(defn socket-server
  [handler port]
  (with-open [server-socket (ServerSocket. port)]
    (println "Listening Port: " (.getLocalPort server-socket))
    (while true
      (let [socket (.accept server-socket)]
        (doto (Thread. #(handler socket)) (.start))))))

(defn ss-proxy
  ""
  []
  (doto (Thread. #(socket-server remote-server 8009)) (.start))
  (socket-server local-server 8008))

(defn test-ss-local
  ""
  []
  (doto (Thread. #(socket-server println-socket 8009)) (.start))
  (socket-server local-server 8008))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
                                        ;  (socket-server socks5-proxy-server 8008)
  (ss-proxy))
