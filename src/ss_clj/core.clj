(ns ss-clj.core
  (:gen-class)
  (:use (ss-clj utils))
  (:require [ss-clj.socks5_server :as s5server]
            [ss-clj.local_server :as lserver]
            [ss-clj.remote_server :as rserver]
            [clojure.tools.cli :as cli])
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
(def local-server #(do (lserver/init-config %) lserver/handle-client-request))
(def remote-server #(do (rserver/init-config %) rserver/handle-client-request))

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

(def cli-options
  ;; An option with a required argument
  [["-m" "--run-mode MODE" "MODEs: socks5-proxy rserver lserver dev-test"
    :default "socks5-proxy"
    :parse-fn #(str %)
    :validate [#(or (= % "socks5-proxy") (= % "lserver") (= % "rserver") (= % "dev-test")) "Must one of: socks5-proxy rserver lserver"]]
   [nil "--rserver-addr RServer-ADDR" "only work when mode==lserver"
    :default "127.0.0.1"
    ]
   [nil "--rserver-port RServer-Port" "only work when mode==lserver"
    :default 8009
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]
    ]
   [nil "--crypto-method Method" "MODEs: plain aes-cbc-128"
    :default "plain"
    :parse-fn #(str %)
    :validate [#(or (= % "plain") (= % "aes-cbc-128")) "Must one of: plain aes-cbc-128"]]
   [nil "--password PASSWORD" "password, now it must 16-bytes"
    :default "0123456789123456"
    :parse-fn #(str %)
    :validate [#(= (count %) 16) "Must 16-bytes"]]
   ["-p" "--port PORT" "Port number"
    :default 8008
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ;; A non-idempotent option
   ["-v" nil "Verbosity level"
    :id :verbosity
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [argument (cli/parse-opts args cli-options)
        mode (get-in argument [:options :run-mode])
        port (get-in argument [:options :port])]
    (cond
      (:errors argument) ; error
      (println (:errors argument))
          
      (get-in argument [:options :help])
      (println (:summary argument)) ; help

      (= mode "dev-test")
      (do (println mode)
          (ss-proxy))

      (= mode "socks5-proxy")                            ; socks5-proxy
      (do (println mode) (socket-server socks5-proxy-server port))

      (= mode "rserver")                ;rserver
      (do (println mode)
          (socket-server (remote-server (:options argument)) port))

      (= mode "lserver")                ;lserver
      (do (println mode)
          (socket-server (local-server (:options argument)) port))
          ))
  )
