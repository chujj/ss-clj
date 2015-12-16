(ns ss-clj.remote_server
  (:gen-class)
  (:use ss-clj.utils)
  (:require [ss-clj.crypto :as crypto])
  (:import (java.net ServerSocket Socket InetSocketAddress UnknownHostException)
           (java.io BufferedReader InputStreamReader InputStream ByteArrayInputStream PrintWriter)))


(def println #(apply clojure.core/println "SERVER: " %&))


(declare parse-dst-addr)

(defn process-openaccess-handshake-request
  "process openaccess handshake-request into a map, like:
  {:open-accesss-key n, :methods n, :atyp n, :dst {:dst-addr :dst-port}}"
  [input-stream]
  (let [open-access-key (read-nbytes input-stream 16) ;open-access-key
        method          (read-byte-to-int input-stream)  ;method
        atyp            (read-byte-to-int input-stream) ;atyp
        addr            (parse-dst-addr atyp input-stream) ; addr
        ]
    {:open-access-key open-access-key :cmd method :atyp atyp :dst addr}))


(def deny-request "deny request"
  (byte-array [0 0 0 1; 4-length
               1; deny
               ]))

(defn parse-dst-addr
  "parse dst addr accord atyp"
  [atyp is]
  (cond
    (= atyp 0x01)                       ;parse ipv4
    (let [addr (read-nbytes is 4)
          port (read-nbytes is 2)]
      (assoc {} :dst-addr addr :dst-port port))
    (= atyp 0x03)                       ;parse domain name
    (let [domain-length (read-byte-to-int is)
          addr (read-nbytes is domain-length)
          port (read-nbytes is 2)]
      (assoc {} :dst-addr addr :dst-port port))
    (= atyp 0x04)                       ;parse ipv6
    (let [addr (read-nbytes is 6)
          port (read-nbytes is 2)]
      (assoc {} :dst-addr addr :dst-port port))))

(def method-not-supported
  (byte-array [
               (unchecked-byte 0x05)    ;Ver
               (unchecked-byte 0x07)    ;rep
               (unchecked-byte 0x00)    ;rsv
               (unchecked-byte 0x01)    ;atype
               (unchecked-byte 0x00)    ;addr0
               (unchecked-byte 0x00)    ;
               (unchecked-byte 0x00)    ;
               (unchecked-byte 0x00)    ;addr3
               (unchecked-byte 0x00)    ;port0
               (unchecked-byte 0x00)    ;port1
               ]))

(defn open-connect
  "return map contain socket and reply code"
  [host port]
  (try
    {:socket (Socket. host port) :rep 0x00}
    (catch UnknownHostException e
      {:socket nil :rep 0x03})
    (catch Exception e
      {:socket nil :rep 0x01})))

(defn build-open-access-reply
  ""
  [request redirect-socket-result]
  (byte-array [0 0 0 1 (unchecked-byte (:rep redirect-socket-result))]))

(defn process-validate-open-access-request
  ""
  [request socket is os]
  (let [cmd (:cmd request)]
    (cond (= cmd 0x1)                   ; Connect
          (do
            (let [dst-addr (get-in request [:dst :dst-addr])
                  dst-port (get-in request [:dst :dst-port])
                  host (bytearray->string dst-addr)
                  port (bytearray->int    dst-port)
                  redirect-socket-result (open-connect host port)]
              (println host)
              (.write os (build-open-access-reply request redirect-socket-result))
              (if (= 0 (:rep redirect-socket-result))
                ;; loop read & write is streams
                (with-open [proxy-socket (:socket redirect-socket-result)
                            client-socket socket
                            pis (.getInputStream proxy-socket)
                            pos (.getOutputStream proxy-socket)
                            cis is
                            cos os]
                  (println "start proxy loop: " (.getLocalPort proxy-socket))
                  ;; (doto (Thread. #(bind-inputstream-to-outputstream pis cos "pis->cos")) (.start))
                  ;; (bind-inputstream-to-outputstream cis pos "cis->pos")
                  (doto (Thread. #(crypto/wraper-encrypto-inputstream-2-outputstream pis cos "pis->cos")) (.start))
                  (crypto/unwraper-encrypto-inputstream-2-outputstream cis pos "cis->pos")
                  (println "end proxy loop"))
                )
              ))
          (= cmd 0x2)                   ; Bind
          (.write os method-not-supported)
          (= cmd 0x3)                   ; UDP Accociate
          (.write os method-not-supported))))

(defn handle-client-request
  ""
  [socket]
  (println "new client request start\n")
  (let [input-stream (.getInputStream socket)
        output-stream (.getOutputStream socket)
        request (process-openaccess-handshake-request input-stream)]
    (println request)
    (if (=                              ; test open-access-key
         (String. (crypto/decrypto (:open-access-key request) crypto/TEST_KEY))
         crypto/TEST_KEY)             
      (process-validate-open-access-request request socket input-stream output-stream)
      (.write output-stream deny-request))
    ))

