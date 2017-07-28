(require '[clojure.string :as str])
(ns alphabet-cipher.coder)

;convert all chars to int 0-25
(defn str-to-ints [s]
  ;map to ascii int, moved to 0-25 range
  (map (fn [x] (- (int x) 97))
    ;convert string to literal list
    (seq (char-array s))))

;convert 0-25 to char
(defn int-to-char [x]
  (char (+ x 97)))

;sum both letters and mod the result
(defn sum-and-mod [x y]
  (mod (+ x y) 26))

;repeat a until its equal or longer than b
(defn repeat-str [a b]
  (apply str
         (repeat
           (Math/ceil
             (/ (count b) (count a)))
           a)))

(defn encode [keyword message]
  (let [
         ;repeat keyword 
         repeated-keyword (repeat-str keyword message)
         ;convert repeated-keyword to ints
         keyword-int (str-to-ints repeated-keyword)
         ;convert message  to ints
         message-int (str-to-ints message)
         ]
    ;map using sum-and-mod
    (apply str
           (map int-to-char
                (map sum-and-mod message-int keyword-int)))))

(defn decode [keyword message]()
  (let [
         ;repeat keyword 
         repeated-keyword (repeat-str keyword message)
         ;convert repeated-keyword to ints
         keyword-int (str-to-ints repeated-keyword)
         ;convert message  to ints
         message-int (str-to-ints message)
         ]
    ;map using sum-and-mod
    (apply str
           (map int-to-char
                (map sum-and-mod message-int keyword-int)))))

(defn decipher [cipher message]
  (let [
         ;repeat keyword 
         repeated-keyword (repeat-str cipher message)
         ;convert repeated-keyword to ints
         keyword-int (str-to-ints repeated-keyword)
         ;convert message  to ints
         message-int (str-to-ints message)
         ]
    ;map using sum-and-mod
    (apply str
           (map int-to-char
                (map sum-and-mod message-int keyword-int)))))