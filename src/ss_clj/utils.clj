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
