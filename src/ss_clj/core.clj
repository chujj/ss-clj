(ns ss-clj.core
  (:gen-class)
  (:import (java.net ServerSocket Socket InetSocketAddress)
           (java.io BufferedReader InputStreamReader InputStream ByteArrayInputStream PrintWriter)))

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

(defn build-connect-relay
  ""
  [request]
  (let [dst-addr (get-in request [:dst :dst-addr] ())
        dst-port (get-in request [:dst :dst-port] ())
        addr-size (count dst-addr)
        port-size (count dst-port)
        domain-addr-size-barray (if (= 3 (:atyp request)) ; one byte for domain addr size count, if atype == 3
                                  [(int2byte (count dst-addr))]
                                  [])]
    (byte-array (+ 1 1 1 1 (count domain-addr-size-barray) addr-size port-size)
                (concat [
                         (unchecked-byte 0x05) ; Ver
                         (unchecked-byte 0x00)     ; rep
                         (unchecked-byte 0x00)     ; rsv
                         (int2byte (:atyp request)) ; atyp
                         ]
                        domain-addr-size-barray           
                        dst-addr
                        dst-port))))

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

(defn bytearray->string
  ""
  [byte-array]
  (reduce str "" (map char byte-array)))

(defn bytearray->int
  "format bytearray to int"
  ([barray]
   (bytearray->int barray (count barray)))
  ([barray count]
   (if (= count 0)
     0
     (let [current_byte_int (byte2int (aget barray 0))
           current_shift (dec count)
           current_byte_result (bit-shift-left current_byte_int (* 8 current_shift))]
       (+ current_byte_result (bytearray->int (byte-array (rest barray)) (dec count)))))))

(defn process-sock5-request
  ""
  [request os]
  (let [cmd (:cmd request)]
    (cond (= cmd 0x1)                   ; Connect
          (do (let [dst-addr (get-in request [:dst :dst-addr])
                    host (bytearray->string dst-addr)
                    ]
                (println host))
            (.write os (build-connect-relay request)))
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
                  (process-sock5-request request output-stream)))
            (.write output-stream deny-request))))

(defn list-sock5-request
  []
  (with-open [server-socket (ServerSocket. 8008)]
    (println "Listening Port: " (.getLocalPort server-socket))
    (while true
      (let [socket (.accept server-socket)]
        (doto (Thread. (handle-client-request socket)) (.start))))))



