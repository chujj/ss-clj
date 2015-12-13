(ns ss-clj.core
  (:gen-class)
  (:import (java.net ServerSocket Socket)
           (java.io BufferedReader InputStreamReader InputStream)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn byte2int [byte]
  (if (< byte 0)
    (int (+ 256 byte))
    (int byte)))

(defn int2byte [int]
  (unchecked-byte int))

(defn read-nbytes
  "read nbytes from input-stream, return byte-array"
  [input-stream count]
  (let [BUFF_SIZE 512
        byte-buffer (byte-array BUFF_SIZE)
        read-count (.read input-stream byte-buffer 0 count)]
    (byte-array read-count byte-buffer)))

(defn read-byte-to-int
  "read nbytes from input-stream, return as int"
  [input-stream]
  (byte2int (aget (read-nbytes input-stream 1) 0)))

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
          (process-socket5-request test-is)))
      {:version 5 :nmethods 2 :methods '(0 1)}))
  (assert ; test under 5
   (= (let [zbuff (byte-array [(byte 0x03) (byte 0x02) (byte 0x00) (byte 0x01)])]
        (with-open [test-is (ByteArrayInputStream. zbuff)]
          (process-socket5-request test-is)))
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

(defn process-sock5-request
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


(defn dump-sock5-request-map
  "print request map"
  [request-map]
  (let [dst-addr (get-in request-map [:dst :dst-addr] nil)
        dst-port (get-in request-map [:dst :dst-port] nil)
        parsed-dst-addr (map #(char %) dst-addr)
        parsed-dst-port (seq dst-port)]
    (println (assoc-in (assoc-in request-map [:dst :dst-addr] parsed-dst-addr)
               [:dst :dst-port] parsed-dst-port))))

(defn list-sock5-request
  []
  (with-open [server-socket (ServerSocket. 8008)]
    (println "Listening Port: " (.getLocalPort server-socket))
    (let [socket (.accept server-socket)
          input-stream (.getInputStream socket)
          output-stream (.getOutputStream socket)
          request (process-sock5-handshake-request input-stream)]
      (println request)
      (if (= (:version request) 5)
        (do (.write output-stream accept-request)
            (let [request (process-sock5-request input-stream)]
              (dump-sock5-request-map request)))
        (.write output-stream deny-request))
      )))




