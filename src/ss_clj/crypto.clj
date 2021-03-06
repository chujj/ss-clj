(ns ss-clj.crypto
  (:gen-class)
  (:use ss-clj.utils)
  (import (java.io ByteArrayInputStream FileInputStream FileOutputStream File ByteArrayOutputStream)
          (javax.crypto Cipher)
          (javax.crypto.spec SecretKeySpec IvParameterSpec)))

(def IV (byte-array 16 '(8)))

(defn encrypto
  ""
  [data key]
  (let [cipher (Cipher/getInstance "AES/CBC/NoPadding" "SunJCE")
        keySpec (SecretKeySpec. (.getBytes key "utf-8") "AES")
        ivp (IvParameterSpec. IV)
        ]
    (do
      (.init cipher Cipher/ENCRYPT_MODE keySpec ivp)
      (.doFinal cipher data))))


(defn decrypto
  ""
  [data key]
  (let [cipher (Cipher/getInstance "AES/CBC/NOPadding" "SunJCE")
        keySpec (SecretKeySpec. (.getBytes key "utf-8") "AES")
        ivp (IvParameterSpec. IV)
        ]
    (do
      (.init cipher Cipher/DECRYPT_MODE keySpec ivp)
      ;(println cipher data)
      (.doFinal cipher data))))

(defn wraper-encrypto-inputstream-2-outputstream
  [is os whoami key]
  (let [package-length 512
        resver-length 2
        payload-length (- package-length resver-length)
        buff (byte-array package-length)
        ]
    (while (let [readed-count (.read is buff resver-length payload-length)
                 readed-count-raw-array (int2bytearray readed-count)
                 continue? (> readed-count 0)
                 ]
             (if continue?
               (do
                 ;; copy resver length
                 (aset-byte buff 0 (aget readed-count-raw-array 0))
                 (aset-byte buff 1 (aget readed-count-raw-array 1))
                 (.write os (encrypto buff key))
                 ))
             continue?))))

(defn blockread-inputstream-align
  [is buff start end]
  (let [readed-count (.read is buff start (- end start))
        continue? (and (> readed-count 0) (< (+ start readed-count) end))]
    (if continue?
      (+ readed-count (blockread-inputstream-align is buff (+ start readed-count) end))
      readed-count)))

(defn unwraper-encrypto-inputstream-2-outputstream
  [is os whoami key]
  (let [package-length 512
        resver-length 2
        payload-length (- package-length resver-length)
        buff (byte-array package-length)
        ]
    (while (let [readed-count (blockread-inputstream-align is buff 0 package-length)
                 after-de-buff (decrypto buff  key)
                 payload-array (byte-array (take resver-length after-de-buff))
                 payload-size (bytearray->int payload-array)
                 continue? (> readed-count 0)
                 ]
             (if continue?
               (do
                 ;; copy resver length
                 (.write os  after-de-buff resver-length payload-size)))
             continue?))))

(def crypto-methods
  {"plain" {:init_fn #()
            :wrapper #(apply bind-inputstream-to-outputstream (take 3 %&))
            :unwraper #(apply bind-inputstream-to-outputstream (take 3 %&))}
   "aes-cbc-128" {:init_fn #(println "no implement")
                  :wrapper wraper-encrypto-inputstream-2-outputstream
                  :unwraper unwraper-encrypto-inputstream-2-outputstream}})

(defn test-wrap-unwrap
  [dataout]
  (let [data dataout
        TEST_KEY "0123456789123456"]
    (with-open [before-en-is (ByteArrayInputStream. (.getBytes data "utf-8"))
                after-en-os (FileOutputStream. (File. "/home/zhujj/tmp2"))]
      (wraper-encrypto-inputstream-2-outputstream before-en-is after-en-os "nothing" TEST_KEY))
    (with-open [before-de-is (FileInputStream. (File. "/home/zhujj/tmp2"))
                after-de-os (FileOutputStream. (File. "/home/zhujj/tmp3"))]
      (unwraper-encrypto-inputstream-2-outputstream before-de-is after-de-os "nothing" TEST_KEY))
    (slurp "/home/zhujj/tmp3")))
