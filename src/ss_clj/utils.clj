(ns ss-clj.utils
  (:gen-class)
  )

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

(defn int2bytearray
  "bytearrays length is 2"
  [int]
  (byte-array [(unchecked-byte (bit-shift-right int 8)) (unchecked-byte int)]))

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

(defn bind-inputstream-to-outputstream
  "read is to buff, write buff to os.
  This Method is block when read or write "
  [is os whoami]
  (let [buff (byte-array 512)]
                                        ;(println whoami "start")
    (while (let [bytes-readed (.read is buff 0 512)
                 continue? (> bytes-readed 0)]
             (if continue?
               (do
                                        ; (println whoami bytes-readed)
                 (.write os buff 0 bytes-readed)
                 (.flush os)))
             continue?))
                                        ;(println whoami "end")
    ))


(defn build-connect-relay
  "build connect relay response package"
  [request redirect-result]
  (byte-array (+ 1 1 1 1 4 2)
              (concat [
                       (unchecked-byte 0x05)                   ; Ver
                       (unchecked-byte (:rep redirect-result))   ; rep
                       (unchecked-byte 0x00)                     ; rsv
                       (unchecked-byte 0x01) ; atyp -> ipv4, always ipv4
                       ]
                      [0 0 0 0]         ;addr
                      (int2bytearray (let [socket (:socket redirect-result)]
                                       (if (= nil socket)
                                         0
                                         (.getLocalPort socket)))))))
