(ns twolions.core
  (:require [hashp.core]))



(def board [["t" "h" "i" "s" "i" "s" "a"]
            ["s" "i" "m" "p" "l" "e" "x"]
            ["b" "x" "x" "x" "x" "e" "b"]
            ["x" "o" "g" "g" "l" "x" "o"]
            ["x" "x" "x" "D" "T" "r" "a"]
            ["R" "E" "P" "E" "A" "d" "x"]
            ["x" "x" "x" "x" "x" "x" "x"]
            ["N" "O" "T" "R" "E" "-" "P"]
            ["x" "x" "D" "E" "T" "A" "E"]])

(def width (count (first board)))
(def height (count board))

(defn all-positions [width height]
  (for [x (range width)
        y (range height)]
    [x y]))

(defn neighbors [width height [x y]]
  (->> (for [dx (range -1 2)
             dy (range -1 2)
             :when (not (and (= dx 0)
                             (= dy 0)))]
         [(+ dx x) (+ dy y)])
       (filter (fn [[x y]]
                 (and (<= 0 x)
                      (<= 0 y)
                      (< x width)
                      (< y height))))))

(defn peek-letter [board [x y]]
  (first (get-in board [y x])))

(defn walk [{:keys [board width height] :as b} word pos]
  (letfn [(acc [letter remaining pos visited]
            (cond
              (and (= letter (peek-letter board pos))
                   (not (seq remaining))) true
              (= letter (peek-letter board pos))
              (let [ns (->> pos
                            (neighbors width height)
                            (remove visited))]
                (->> ns
                     (map (fn [n] (acc (first remaining) (rest remaining) n (conj visited pos))))
                     (some true?)))
              :else false))]
    (acc (first word) (rest word) pos #{})))

(defn solve [board word]
  (let [height (count board)
        width (count (first board))
        positions (all-positions width height)]
    (boolean (->> positions
                  (map (partial walk {:board  board
                                      :width  width
                                      :height height}
                                word))
                  (some true?)))))

(def tests
  [{
    :name   "test 1"
    :input  {:board [["t" "h" "i" "s" "i" "s" "a"]
                     ["s" "i" "m" "p" "l" "e" "x"]
                     ["b" "x" "x" "x" "x" "e" "b"]
                     ["x" "o" "g" "g" "l" "x" "o"]
                     ["x" "x" "x" "D" "T" "r" "a"]
                     ["R" "E" "P" "E" "A" "d" "x"]
                     ["x" "x" "x" "x" "x" "x" "x"]
                     ["N" "O" "T" "R" "E" "-" "P"]
                     ["x" "x" "D" "E" "T" "A" "E"]]
             :word  "this"}
    :output true}
   {:name   "test 2"
    :input  {:board [["t" "h" "i" "s" "i" "s" "a"]
                     ["s" "i" "m" "p" "l" "e" "x"]
                     ["b" "x" "x" "x" "x" "e" "b"]
                     ["x" "o" "g" "g" "l" "x" "o"]
                     ["x" "x" "x" "D" "T" "r" "a"]
                     ["R" "E" "P" "E" "A" "d" "x"]
                     ["x" "x" "x" "x" "x" "x" "x"]
                     ["N" "O" "T" "R" "E" "-" "P"]
                     ["x" "x" "D" "E" "T" "A" "E"]]
             :word  "not"}
    :output false}
   {:name   "test 3"
    :input  {:board [["t" "h" "i" "s" "i" "s" "a"]
                     ["s" "i" "m" "p" "l" "e" "x"]
                     ["b" "x" "x" "x" "x" "e" "b"]
                     ["x" "o" "g" "g" "l" "x" "o"]
                     ["x" "x" "x" "D" "T" "r" "a"]
                     ["R" "E" "P" "E" "A" "d" "x"]
                     ["x" "x" "x" "x" "x" "x" "x"]
                     ["N" "O" "T" "R" "E" "-" "P"]
                     ["x" "x" "D" "E" "T" "A" "E"]]
             :word  "board"}
    :output true}
   {:name   "test 4"
    :input  {:board [["t" "h" "i" "s" "i" "s" "a"]
                     ["s" "i" "m" "p" "l" "e" "x"]
                     ["b" "x" "x" "x" "x" "e" "b"]
                     ["x" "o" "g" "g" "l" "x" "o"]
                     ["x" "x" "x" "D" "T" "r" "a"]
                     ["R" "E" "P" "E" "A" "d" "x"]
                     ["x" "x" "x" "x" "x" "x" "x"]
                     ["N" "O" "T" "R" "E" "-" "P"]
                     ["x" "x" "D" "E" "T" "A" "E"]]
             :word  "simple"}
    :output true}
   {:name   "test 5"
    :input  {:board [["t" "h" "i" "s" "i" "s" "a"]
                     ["s" "i" "m" "p" "l" "e" "x"]
                     ["b" "x" "x" "x" "x" "e" "b"]
                     ["x" "o" "g" "g" "l" "x" "o"]
                     ["x" "x" "x" "D" "T" "r" "a"]
                     ["R" "E" "P" "E" "A" "d" "x"]
                     ["x" "x" "x" "x" "x" "x" "x"]
                     ["N" "O" "T" "R" "E" "-" "P"]
                     ["x" "x" "D" "E" "T" "A" "E"]]
             :word  "REPEATED"}
    :output false}
   {:name   "test 6"
    :input  {:board [["t" "h" "i" "s" "i" "s" "a"]
                     ["s" "i" "m" "p" "l" "e" "x"]
                     ["b" "x" "x" "x" "x" "e" "b"]
                     ["x" "o" "g" "g" "l" "x" "o"]
                     ["x" "x" "x" "D" "T" "r" "a"]
                     ["R" "E" "P" "E" "A" "d" "x"]
                     ["x" "x" "x" "x" "x" "x" "x"]
                     ["N" "O" "T" "R" "E" "-" "P"]
                     ["x" "x" "D" "E" "T" "A" "E"]]
             :word  "NOTRE-PEATED"}
    :output true}
   {:name   "test 7"
    :input  {:board [["y" "g" "f" "y" "e" "i"]
                     ["c" "o" "r" "p" "o" "u"]
                     ["j" "u" "z" "s" "e" "l"]
                     ["s" "y" "u" "r" "h" "p"]
                     ["e" "a" "e" "g" "n" "d"]
                     ["h" "e" "l" "s" "a" "t"]]
             :word  "yours"}
    :output true}
   {:name   "test 8"
    :input  {:board [["y" "g" "f" "y" "e" "i"]
                     ["c" "o" "r" "p" "o" "u"]
                     ["j" "u" "z" "s" "e" "l"]
                     ["s" "y" "u" "r" "h" "p"]
                     ["e" "a" "e" "g" "n" "d"]
                     ["h" "e" "l" "s" "a" "t"]]
             :word  "sana"}
    :output false}
   {:name   "test 9"
    :input  {:board [["y" "g" "f" "y" "e" "i"]
                     ["c" "o" "r" "p" "o" "u"]
                     ["j" "u" "z" "s" "e" "l"]
                     ["s" "y" "u" "r" "h" "p"]
                     ["e" "a" "e" "g" "n" "d"]
                     ["h" "e" "l" "s" "a" "t"]]
             :word  "san"}
    :output true}
   {:name   "test 10"
    :input  {:board [["y" "g" "f" "y" "e" "i"]
                     ["c" "o" "r" "p" "o" "u"]
                     ["j" "u" "z" "s" "e" "l"]
                     ["s" "y" "u" "r" "h" "p"]
                     ["e" "a" "e" "g" "n" "d"]
                     ["h" "e" "l" "s" "a" "t"]]
             :word  "danger"}
    :output true}
   {:name   "test 11"
    :input  {:board [["y" "g" "f" "y" "e" "i"]
                     ["c" "o" "r" "p" "o" "u"]
                     ["j" "u" "z" "s" "e" "l"]
                     ["s" "y" "u" "r" "h" "p"]
                     ["e" "a" "e" "g" "n" "d"]
                     ["h" "e" "l" "s" "a" "t"]]
             :word  "help"}
    :output true}
   {:name   "test 12"
    :input  {:board [["y" "g" "f" "y" "e" "i"]
                     ["c" "o" "r" "p" "o" "u"]
                     ["j" "u" "z" "s" "e" "l"]
                     ["s" "y" "u" "r" "h" "p"]
                     ["e" "a" "e" "g" "n" "d"]
                     ["h" "e" "l" "s" "a" "t"]]
             :word  "vomit"}
    :output false}])

(doseq [{name          :name
         {board :board
          word  :word} :input
         output        :output} tests]
  (let [got (solve board word)
        exp output]
    (when (not= got exp)
      (println (str "Test Name: " name "\n"
                    "Expected: " exp "\n"
                    "Got: " got "\n")))))