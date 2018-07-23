(ns permutation-test.core)

;(require '[clojure.java.io :as io])
(require '[clojure.math.numeric-tower :as math])

(defn permute-rec
  "Creates 1 permutation of samples a and b.

  Note that this does not create the full set of all possible permutations"
  [a b]
  (loop [b-perm []
         a-perm (concat a b)]
    (if (= (count a-perm) (count b-perm))
      (list a-perm b-perm)
      (let [dummy (shuffle a-perm)]
        (println dummy)
        (recur (cons (first dummy) b-perm) (rest dummy))))))

(defn permute
  "The easy, clojure-esk way
  Save time by concatenating only once
  "
  [sample]
  (partition (/ (count sample) 2) (shuffle sample)))

(defn mean
  "Mean of a sample"
  [sample]
  (double (/ (reduce + sample) (count sample))))

(defn ssqd
  "Sum of squared distances"
  [sample]
  (let [mu (mean sample)]
    (reduce + (map #(reduce * (repeat 2 (- % mu))) sample))))

(defn variance
  "Variance of a sample"
  [sample]
  (/ (ssqd sample) (count sample)))

(defn std
  "Standard deviation of a sample"
  [sample]
  (math/sqrt (variance sample)))

(defn percentile
  "perc has to be a number within [0 1] interval"
  [sample perc]
  (nth (sort sample) (* perc (count sample))))

(defn describe
  [sample]
  (println "Min: " (apply min sample))
  (println "25%: " (percentile sample 0.25))
  (println "Median: " (percentile sample 0.5))
  (println "Mean: " (mean sample))
  (println "75%: " (percentile sample 0.75))
  (println "Max: " (apply max sample))
  (println "Std: " (std sample) "\n"))

(defn test-diff
  [a b]
  (/ (- (mean a) (mean b))
     (+ (std a) (std b))))

(defn create-test-dist
  "Create the distribution of test results by running the test across n different permutations"
  [both n]
  (for [i (range n)]
    (let [perm (permute both)]
      (test-diff (first perm) (second perm)))))

(defn count-test-dist
  "Alternative to keeping the entire test distribution:
  Just create each element and count it if it is larger than the real diff"
  [both n real-diff]
  (transduce (filter #(> % real-diff)) + 0
             (for [i (range n)]
               (let [perm (permute both)]
                 (test-diff (first perm) (second perm))))))


;alternative versions of final execution
(defn parallel-with-dist
  [both n tc]
  (let [threads 
        (for [i (range tc)]
          (future (doall (create-test-dist both (/ n tc)))))]
    (reduce concat (map deref threads))))

(defn parallel-no-dist
  [both n tc real-diff]
  (let [threads 
        (for [i (range tc)]
          (future (count-test-dist both (/ n tc) real-diff)))]
    (reduce + (map deref threads))))

(def thread-count 8)

(defn -main
  []
  (time
    (let [a (take 1000 (repeatedly #(rand-int 10)))
          b (map #(+ 0.1 %) (take 1000 (repeatedly #(rand-int 10))))
          both (concat a b)
          real-diff (test-diff a b)]
      (describe a)
      (describe b)
      ;parallel version
      (def test-dist (parallel-with-dist both 10000 thread-count))
      (describe test-dist)
      (println (double (/ (transduce (filter #(> % real-diff)) + 0 test-dist)
                          (count test-dist))))
      ;(def test-cnt (parallel-no-dist both 10000 thread-count real-diff))
      ;(println (double (/ test-cnt 10000)))
      (shutdown-agents) ;shut down threads
      (println "Real test-value: " real-diff))))
