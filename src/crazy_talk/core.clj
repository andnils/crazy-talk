(ns crazy-talk.core
  (:require [clojure.set :refer [union]]
            [clojure.string :as s]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1
;; Create a word chain from a string.

(defn word-chain [word-transitions]
  (reduce
   (fn [r t] (merge-with union r
                        (let [[a b c] t]
                          {[a b] (if c #{c} #{})})))
   {}
   word-transitions))


(defn text->word-chain
  "Given a text (ordinary string), create a map
  with keys consisting of two words (a vector with two strings),
  and values consisting of a set of strings with the possible
  continuations.

  E.g: (text->word-chain \"once upon a time\")
  {[\"time\" nil] #{}, 
   [\"a\" \"time\"] #{}, 
   [\"upon\" \"a\"] #{\"time\"}, 
   [\"once\" \"upon\"] #{\"a\"}}"
  [s]
  (let [words (s/split s #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2
;; Now when we have a word chain we
;; can use it to generate text.

(defn ends-with-punctuation? [[_ s]]
  (boolean (re-matches #".*[\.!?] *$" s)))

(defn walk-chain [prefix chain result]
  (let [candidates (get chain prefix)]
    (if (or (empty? candidates)
            (ends-with-punctuation? prefix))
      result
      (let [candidate (rand-nth (seq candidates))
            new-prefix [(last prefix) candidate]]
        (recur new-prefix chain (conj result candidate))))))


(defn generate-text
  "Given a start-phrase (a string with exactly 2 words) and a
  word chain (which is a map with keys [word1 word2] to value
  (set of possible follow-up words for word1 and word2),
  generate a sentence."
  [start-phrase word-chain]
  (let [prefix (s/split start-phrase #" ")
        result-chain (walk-chain prefix word-chain prefix)]
    (s/join " " result-chain)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3
;; Get text from files

(defn process-gzip-file [filename]
  (with-open [in (java.util.zip.GZIPInputStream.
                  (clojure.java.io/input-stream (str "resources/" filename)))]
    (text->word-chain (slurp in))))

(defn make-chain-from-files [files]
  (apply merge-with union (map process-gzip-file files)))




(defn random-prefix
  "Given a word-chain, try to find a proper starting point,
  i.e. two words where the first word is upper case."
  [chain]
  (->> (keys chain)
       (filter (fn [[word _]] (re-matches #"^[A-Z].*[^\.!?]$" word)))
       (rand-nth)
       (s/join " ")))



(def gzip-files ["quantum.txt.gz"])

(def en-files
  ["lotr2.txt.gz" "quantum.txt.gz" "starwars2.txt.gz" "ring-data.txt.gz"])

(def swe-files
  ["beatles.txt.gz" "haskell.txt.gz" "kalle.txt.gz" "linux.txt.gz"
   "lotr.txt.gz" "pippi.txt.gz" "sociala_medier.txt.gz" "vard.txt.gz"
   "starwars.txt.gz"])

(def ^:dynamic *files* swe-files)


;;;;;;;;;;;;;;;;;;;;
;; Section 4
;; Main entry point

(defn crazy-text []
  (let [word-chain (make-chain-from-files *files*)
        prefix (random-prefix word-chain)]
    (generate-text prefix word-chain)))




(comment


  (binding [*files* en-files]
    (crazy-text))
  
  )
