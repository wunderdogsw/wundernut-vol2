(ns wunderdog.core
  (:require [clojure.math.combinatorics :refer [combinations]]))

;; Wunderdogin koodauspähkinä 2015
;; Muhkeimmat sanaparit
;; Ratkaisun totetus: Ari Paasonen (paasar ät geemail piste com)

;; Kutsumalla resolve-and-print-muhkeimmat-parit saa tuloksen ruudulle.
;; Pahoittelen sekakielisyyttä, mutta muhkeus on kiva sana =)

(defn read-data []
  (slurp "alastalon_salissa.txt"))

(defn as-words [input-str]
  (re-seq #"[åäöÅÄÖ\w'-]+" input-str))

(defn unique-letters [word]
  (sort (list* (filter (set (seq "abcdefghijklmnopqrstuvwxyzåäö"))
                       (set (seq (clojure.string/lower-case word)))))))

(defn group-by-unique-letters [words]
  (group-by unique-letters words))

(defn contains-all? [aset bset]
  (empty? (filter nil? (map #(aset %) bset))))

(defn accumulator-has-similar [existing potential]
  (let [pot-set (set potential)]
    (first (filter #(let [existing (set %)]
                      (or (contains-all? existing pot-set)
                          (contains-all? pot-set existing)))
                   existing))))

(defn replace-if-muhkeampi [aset old new]
  (if (> (count new) (count old))
    (-> aset
        (disj old)
        (conj new))
    aset))

(defn resolve-muhkeimmat [muhkeus-keys]
  (reduce (fn [acc potential]
            (let [similar (accumulator-has-similar acc potential)]
              (if similar
                (replace-if-muhkeampi acc similar potential)
                (conj acc potential))))
          #{}
          muhkeus-keys))

(defn create-word-pairs [muhkeimmat]
  (combinations muhkeimmat 2))

(defn muhkeus-arvo [[f s]]
  (count (set (concat f s))))

(defn sort-by-muhkeus [pairs]
  (sort-by (fn [pair] (muhkeus-arvo pair)) > pairs))

(defn get-all-with-highest-value [sorted-by-muhkeus]
  (let [muhkein-arvo (muhkeus-arvo (first sorted-by-muhkeus))]
    (filter (fn [pari]
              (= muhkein-arvo (muhkeus-arvo pari)))
            sorted-by-muhkeus)))

(defn print-result [grouped-by-unique-letters muhkeimmat]
  (print
   (str "Suurin muhkeusarvo: " (muhkeus-arvo (first muhkeimmat))
        "\n\nNämä parit toteuttavat sen:\n\n"
         (clojure.string/join "\n"
           (map (fn [[f s]] (str (first f) "\t" (first s)))
                (map (fn [[f s]] [(set (grouped-by-unique-letters f))
                                 (set (grouped-by-unique-letters s))])
                  muhkeimmat))))))

(defn resolve-and-print-muhkeimmat-parit []
  (let [grouped-by-unique-letters (-> (read-data)
                                      as-words
                                      group-by-unique-letters)
        muhkeimmat-parit (-> (resolve-muhkeimmat (keys grouped-by-unique-letters))
                             create-word-pairs
                             sort-by-muhkeus
                             get-all-with-highest-value)]
    (print-result grouped-by-unique-letters
                  muhkeimmat-parit)))
