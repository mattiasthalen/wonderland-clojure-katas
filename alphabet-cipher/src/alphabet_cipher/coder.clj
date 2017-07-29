(require '[clojure.string :as str])
(ns alphabet-cipher.coder)

;convert the char to int
(defn char-to-int [c]
  ;convert to int, then subtract 97 to move from ascii range to 0-25
  (- (int c) 97))

;convert int to char
(defn int-to-char [i]
  ;add 97 to int to move to ascii range, then convert it to char
  (char (+ i 97)))

;define encode function to encode the message using a keyword
(defn encode [keyword message]
  (let [ ;repeat keyword
         repeated-keyword (apply str (repeat (Math/ceil (/ (count message) (count keyword))) keyword))
         ;convert repeated-keyword to ints
         keyword-int (map char-to-int (seq repeated-keyword))
         ;convert message to ints
         message-int (map char-to-int (seq message))]
    
    ;leverage ints against each other and spit out the encoded message
    (->>
      ;sum message- int & keyword-int
      (map + message-int keyword-int)
      ;mod the result
      (map #(mod % 26) ,,,)
      ;convert ints to string
      (map int-to-char ,,,)
      ;make string from list
      (apply str))))

;define decode function to decode the message using a keyword
(defn decode [keyword message]
  (let [ ;repeat keyword
         repeated-keyword (apply str (repeat (Math/ceil (/ (count message) (count keyword))) keyword))
         ;convert repeated-keyword to ints
         keyword-int (map char-to-int (seq repeated-keyword))
         ;convert message to ints
         message-int (map char-to-int (seq message))]
    
    ;leverage ints against each other and spit out the decoded message
    (->>
      ;subtract message-int by keyword-int
      (map - message-int keyword-int)
      ;mod the result
      (map #(mod % 26) ,,,)
      ;convert ints to string
      (map int-to-char ,,,)
      ;make string from list
      (apply str))))

;define decipher funtion to find the keyword used for encoding
(defn decipher [cipher message]
  (let [ ;decode to keyword string
         key-string (decode message cipher)]
    
    ;iterate through key-string to find keyword
    (loop [i 1]
      ;set keyword from 0 to current iteration
      (let [keyword (subs key-string 0 i)]
      ;check if encoding message with current keyword results in the correct cipher
      (if (= (encode keyword message) cipher)
        ;return keyword if the correct keyword is found
        keyword
        ;if not correct keyword, increase next iteration
        (recur (inc i)))))))