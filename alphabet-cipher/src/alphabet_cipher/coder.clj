(require '[clojure.string :as str])
(ns alphabet-cipher.coder)

;convert all chars to int 0-25
(defn str-to-ints [s]
  ;map to ascii int, moved to 0-25 range
  (map (fn [x] (- (int x) 97))
    ;convert string to literal list
    (seq (char-array s))))

(defn encode [keyword message]
  (let [
         ;repeat keyword util it equal or longer than the message
         repeated-keyword (apply str (repeat (Math/ceil (/ (count message) (count keyword))) keyword))
         keyword-int (str-to-ints repeated-keyword)
         message-int (str-to-ints message)
         ]
    (map + message-int keyword-int)))

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")