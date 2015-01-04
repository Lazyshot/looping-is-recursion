(ns looping-is-recursion)

(defn power [base exp]
  (let [helper
        (fn [acc base exp]
          (if (zero? exp)
            acc
            (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper
        (fn [i a-seq]
          (if (empty? a-seq) i
            (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper
        (fn [seq1 seq2]
          (cond
            (and (empty? seq1) (empty? seq2)) true
            (or (empty? seq1) (empty? seq2)) false
            (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
            :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) i
      :else (recur (inc i) (rest s)))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         s a-seq]
    (if (empty? s)
      (/ sum n)
      (recur (inc n) (+ sum (first s)) (rest s)))))

(defn parity [a-seq]
  (loop [r #{}
         s a-seq]
    (cond
      (empty? s) r
      (contains? r (first s)) (recur (disj r (first s)) (rest s))
      :else (recur (conj r (first s)) (rest s)))))


(defn fast-fibo [n]
  (if (< n 2) n
    (loop [f0 1
           f1 0
           x n]
      (if (= x 1)
        f0
        (recur (+ f0 f1) f0 (dec x))))))

(defn cut-at-repetition [a-seq]
  (loop [ls #{}
         r []
         s a-seq]
    (if (or (empty? s) (contains? ls (first s))) r
      (recur (conj ls (first s)) (conj r (first s)) (rest s)))))

