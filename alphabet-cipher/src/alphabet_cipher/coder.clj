(require '[clojure.string :as str])
(ns alphabet-cipher.coder)

;define encode function to encode the message using a keyword
(defn encode [keyword message & [decode]]
  (let [ ;repeat keyword to be at least as long as the message
         repeated-keyword (apply str (repeat (Math/ceil (/ (count message) (count keyword))) keyword))
        
         ;convert string to list of ints
         str-to-ints (fn [s]
                       (->>
                         ;make string a list of chars
                         (seq  s)
                         ;map chars to ints
                         (map int ,,,)
                         ;subtract 97 from ints to set the range to 0-25
                         (map #(- % 97) ,,,)))
         
         ;convert repeated-keyword to ints
         keyword-int (str-to-ints repeated-keyword)
        
         ;convert message to ints
         message-int (str-to-ints message)]
    
    ;leverage ints against each other and spit out the message
    (->>
      ;check if decode mode is selected
      (if decode
        ;if decoding, subtract keyword-int from message-int
        (map - message-int keyword-int)
        
        ;if not decoding, sum message-int & keyword-int
        (map + message-int keyword-int))
     
      ;mod the result so z+1=a
      (map #(mod % 26) ,,,)
     
      ;add 97 to ints to go back to ascii range
      (map #(+ % 97) ,,,)
     
      ;convert ints to chars
      (map char ,,,)
     
      ;make string from list
      (apply str ,,,))))

;define decode function to decode the message using a keyword
(defn decode [keyword message]
  ;call encode function with decode set to true
  (encode keyword message true))

;define decipher funtion to find the keyword used for encoding
(defn decipher [cipher message]
  (let [ ;decode to keyword string
         key-str (decode message cipher)]
    
    ;iterate through substrings of key-str to find keyword
    (loop [i 1]
      ;set keyword to substring of key-str from 0 to current iteration
      (let [keyword (subs key-str 0 i)]
        
      ;check if encoding message with current keyword results in the correct cipher
      (if (= (encode keyword message) cipher)
        ;return keyword if the correct keyword is found
        keyword
        
        ;if not correct keyword, increase next iteration
        (recur (inc i)))))))
