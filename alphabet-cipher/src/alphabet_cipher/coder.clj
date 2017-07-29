(require '[clojure.string :as str])
(ns alphabet-cipher.coder)

(defn encode [keyword message]
  (let [ ;convert the string into a list of ints
         str-to-ints (fn [s]
                       (->>
                         ;make the string a list of literals
                         (seq s)
                         ;map the literals to int
                         (map int ,,,)
                         ;subtract 97 to make the range 0-25
                         (map #(- % 97) ,,,)))
         
         ;repeat keyword
         repeated-keyword (apply str (repeat (Math/ceil (/ (count message) (count keyword))) keyword))
         ;convert repeated-keyword to ints
         keyword-int (str-to-ints repeated-keyword)
         ;convert message to ints
         message-int (str-to-ints message)]

    (->>
      ;sum int by int
      (map + message-int keyword-int)
      ;mod the result
      (map #(mod % 26) ,,,)
      ;add 97 to map to ascii
      (map #(+ 97 %) ,,,)
      ;convert to char
      (map char ,,,)
      ;conver to string
      (apply str ,,,))))

(defn decode [keyword message]
  (let [ ;convert the string into a list of ints
         str-to-ints (fn [s]
                       (->>
                         ;make the string a list of literals
                         (seq s)
                         ;map the literals to int
                         (map int ,,,)
                         ;subtract 97 to make the range 0-25
                         (map #(- % 97) ,,,)))
         
         ;repeat keyword
         repeated-keyword (apply str (repeat (Math/ceil (/ (count message) (count keyword))) keyword))
         ;convert repeated-keyword to ints
         keyword-int (str-to-ints repeated-keyword)
         ;convert message to ints
         message-int (str-to-ints message)]

    (->>
      ;sub int by int
      (map - message-int keyword-int)
      ;mod the result
      (map #(mod % 26) ,,,)
      ;add 97 to map to ascii
      (map #(+ 97 %) ,,,)
      ;convert to char
      (map char ,,,)
      ;conver to string
      (apply str ,,,))))

(defn decipher [cipher message]
  (let [ ;decode to keyword string
         key-string (decode message cipher)]
    (loop [i 1]
      (if (= (encode (subs key-string 0 i) message) cipher)
        (subs key-string 0 i)
        (recur (inc i))))))