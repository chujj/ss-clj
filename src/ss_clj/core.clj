(ns ss-clj.core
  (:gen-class)
  (:import (java.net ServerSocket Socket)
           (java.io BufferedReader InputStreamReader InputStream)))

(def HANDSHAKE_VERSION_BYTE_LENGTH 1)
(def HANDSHAKE_NMETHODS_BYTE_LENGTH 1)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn read-nbytes
  "read nbytes from input-stream, return byte-array"
  [input-stream count]
  (let [BUFF_SIZE 512
        byte-buffer (byte-array BUFF_SIZE)
        read-count (.read input-stream byte-buffer 0 count)]
    (byte-array read-count byte-buffer)))

(defn read-handshake-version
  [input-stream]
  (aget (read-nbytes input-stream HANDSHAKE_VERSION_BYTE_LENGTH) (dec HANDSHAKE_VERSION_BYTE_LENGTH)))

(defn process-sock5-request
  "process sock5 request into a map, like:
  {:version n, :nmethods n, :methods '(& n)}"
  [input-stream]
  (let [parsed-request (assoc {} :version (read-handshake-version input-stream))]
    (if (= (:version parsed-request) 5)
      (let [nmethods (aget (read-nbytes input-stream HANDSHAKE_NMETHODS_BYTE_LENGTH) (dec HANDSHAKE_NMETHODS_BYTE_LENGTH))
            methods (read-nbytes input-stream nmethods)]
        (assoc parsed-request :nmethods nmethods :methods (seq methods)))
      parsed-request)))

(def accept-request "only support unauthorized"
  (byte-array [(byte 0x05) (byte 0x00)]))
(def deny-request "deny request"
  (byte-array [(byte 0x05) (unchecked-byte 0xff)]))

(defn list-sock5-request
  []
  (with-open [server-socket (ServerSocket. 8008)]
    (println "Listening Port: " (.getLocalPort server-socket))
    (let [socket (.accept server-socket)
          input-stream (.getInputStream socket)
          output-stream (.getOutputStream socket)
          request (process-sock5-request input-stream)]
      (println request)
      (if (= (:version request) 5)
        (do (.write output-stream accept-request)
            )
        (.write output-stream deny-request))
      )))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-process-sock5-request
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
