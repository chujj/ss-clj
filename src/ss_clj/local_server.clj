(ns ss-clj.local_server
  (:gen-class)
  (:use ss-clj.utils)
  (:require [ss-clj.crypto :as crypto])
  (:import (java.net ServerSocket Socket InetSocketAddress UnknownHostException)
           (java.io BufferedReader InputStreamReader InputStream ByteArrayInputStream PrintWriter)))


(def println #(apply clojure.core/println "LOCAL: " %&))

(def local-config nil)

(defn process-sock5-handshake-request
  "process sock5 handshake-request into a map, like:
  {:version n, :nmethods n, :methods '(& n)}"
  [input-stream]
  (let [parsed-request (assoc {} :version (read-byte-to-int input-stream))]
    (if (= (:version parsed-request) 5)
      (let [nmethods (read-byte-to-int input-stream)
            methods (read-nbytes input-stream nmethods)]
        (assoc parsed-request :nmethods nmethods :methods (seq methods)))
      parsed-request)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-process-sock5-handshake-request
  "Test Function"
  []
  (assert ; test two method
   (= (let [zbuff (byte-array [(byte 0x05) (byte 0x02) (byte 0x00) (byte 0x01)])]
        (with-open [test-is (ByteArrayInputStream. zbuff)]
          (process-sock5-handshake-request test-is)))
      {:version 5 :nmethods 2 :methods '(0 1)}))
  (assert ; test under 5
   (= (let [zbuff (byte-array [(byte 0x03) (byte 0x02) (byte 0x00) (byte 0x01)])]
        (with-open [test-is (ByteArrayInputStream. zbuff)]
          (process-sock5-handshake-request test-is)))
      {:version 3})))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def accept-request "only support unauthorized"
  (byte-array [(byte 0x05) (byte 0x00)]))
(def deny-request "deny request"
  (byte-array [(byte 0x05) (unchecked-byte 0xff)]))

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

(defn parse-sock5-request
  "process sock5 request into map, like"
  [is]
  (let [version (read-byte-to-int is)
        cmd (read-byte-to-int is)
        rsv (read-byte-to-int is)
        atyp (read-byte-to-int is)
        dst-addr (parse-dst-addr atyp is)]
    (assoc {}
           :version version
           :cmd cmd
           :rsv rsv
           :atyp atyp
           :dst dst-addr)))

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

(defn open-connect-with-remote-server
  "open remote proxy socket, return map contain socket and reply code
  bytes format is:
Req:
| open-assecss-key        | method | atyp | addr | port |
|      16                 | 1      |  1   | n    | 2    |

Resp:
| 4length | result      |
| 4       | n (4length) |
"
  [open-access-key method atyp dst-addr dst-port]
  (try
    (let [remote-socket (Socket. (:rserver-addr local-config) (:rserver-port local-config))
          is (.getInputStream remote-socket)
          os (.getOutputStream remote-socket)
          domain-addr-prefix-barray (if (= atyp 3) (byte-array 1 [(unchecked-byte (count dst-addr))]) [])
          req-package (byte-array (concat (if (= "plain" (:crypto-method local-config)) (byte-array 16)
                                              (crypto/encrypto open-access-key (:password local-config)))
                                          (byte-array [(int2byte method)])
                                          (byte-array [(int2byte atyp)])
                                          domain-addr-prefix-barray
                                          dst-addr
                                          dst-port))
          read-buff (byte-array 4)]
      (println "query remote : "  (seq req-package))
      (.write os req-package)
      (.read is read-buff 0 4)
      (let [resp-length (bytearray->int read-buff)
            resp-read-buff (byte-array resp-length)
            resp-payload-length (.read is resp-read-buff 0 resp-length)
            resp-rsp-code (bytearray->int resp-read-buff)]
        (println "response of remote : "  resp-length (seq resp-read-buff))
        ; TODO parse buff. For now we just translate to int
        (if (and (= resp-payload-length resp-length) ; desire length == read length && code == 0x00
                 (= resp-rsp-code 0x0))
          {:socket remote-socket :rep 0x00}
          {:socket nil :rep 0x1})))
    (catch UnknownHostException e
      {:socket nil :rep 0x03})
    (catch Exception e
      (do (println (.getMessage e)) {:socket nil :rep 0x01}))))

(defn process-sock5-request
  ""
  [request socket is os]
  (let [cmd (:cmd request)]
    (cond (= cmd 0x1)                   ; Connect
          (let [dst-addr (get-in request [:dst :dst-addr])
                dst-port (get-in request [:dst :dst-port])
                method 1
                atyp (:atyp request)
                password (:password local-config)
                open-access-key (.getBytes password "utf-8")
                redirect-socket-result (open-connect-with-remote-server open-access-key method atyp dst-addr dst-port)
                ]
            (println "connect done")
            (println redirect-socket-result)
            (.write os (build-connect-relay request redirect-socket-result))
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
                (doto (Thread. #((get-in crypto/crypto-methods [(:crypto-method local-config) :unwraper])  pis cos "pis->cos" password)) (.start))
                ((get-in crypto/crypto-methods [(:crypto-method local-config) :wrapper]) cis pos "cis->pos" password)

                (println "end proxy loop"))
              ))
          (= cmd 0x2)                   ; Bind
          (.write os method-not-supported)
          (= cmd 0x3)                   ; UDP Accociate
          (.write os method-not-supported))))


(defn dump-sock5-request-map
  "print request map"
  [request-map]
  (let [dst-addr (get-in request-map [:dst :dst-addr] nil)
        dst-port (get-in request-map [:dst :dst-port] nil)
        parsed-dst-addr (bytearray->string dst-addr)
        parsed-dst-port (bytearray->int dst-port)]
    (println (assoc-in (assoc-in request-map [:dst :dst-addr] parsed-dst-addr)
               [:dst :dst-port] parsed-dst-port))))

(defn handle-client-request
  ""
  [socket]
  (println "new client request start\n")
  (let [input-stream (.getInputStream socket)
        output-stream (.getOutputStream socket)
        request (process-sock5-handshake-request input-stream)]
    (println request)
    (if (= (:version request) 5)
      (do (.write output-stream accept-request)
          (let [request (parse-sock5-request input-stream)]
            (dump-sock5-request-map request)
            (process-sock5-request request socket input-stream output-stream)))
      (.write output-stream deny-request))))

(defn init-config
  [config-map]
  (def local-config config-map))
