(ns snip.core)

;; binary search

(defn make-search
  [& {:keys [id cmp] :or {id identity, cmp <}}]
  (let [c (comparator cmp)]
    (fn search
      ([v x]
       (search v x 0 (count v)))
      ([v x start end]
       (let [f (fn f [start end edge]
                 (if (== start end)
                   (if (nil? edge)
                     [start start]
                     start)
                   (let [i (quot (+ start end) 2)
                         r (c x (id (v i)))]
                     (cond
                       (and (== r 0) (nil? edge))
                       [(f start i :left) (f (inc i) end :right)]

                       (or (> r 0)
                           (and (== r 0) (= edge :right)))
                       (recur (inc i) end edge)

                       (or (< r 0)
                           (and (== r 0) (= edge :left) ))
                       (recur start i edge)))))]
         (f start end nil))))))

;; disjoint set

(defn ds-find
  [ds k]
  (loop [ds ds, k k, ks []]
    (let [v (ds k)]
      (if (or (nil? v) (= v k))
        (let [ds (reduce (fn [ds pk]
                           (assoc ds pk k))
                         ds ks)]
          [ds k])
        (recur ds v (conj ks k))))))

(defn ds-union
  [ds a b]
  (let [[ds av] (ds-find ds a)
        [ds bv] (ds-find ds b)]
    (assoc ds bv av)))

;; primes

(defn prime?
  [n]
  (cond
    (<= n 3)
    (> n 1)

    (or (zero? (rem n 2))
        (zero? (rem n 3)))
    false

    :else
    (loop [i 5]
      (if (<= (* i i) n)
        (if (or (zero? (rem n i))
                (zero? (rem n (+ i 2))))
          false
          (recur (+ i 6)))
        true))))

(def p-max (-> 1000000000 Math/sqrt inc long))

(def primes
  (filter prime? (range 2 (inc p-max))))

;; not as fast
#_
(def primes
  (let [t (boolean-array (inc p-max))
        f (fn f
            [i]
            (doseq [i (->> (iterate #(+ % i) i)
                           (take-while #(<= % p-max)))]
              (aset t i true))
            (let [i (some (fn [i]
                            (if (not (aget t i)) i))
                          (range (inc i) (inc p-max)))]
              (if (some? i)
                (lazy-seq (cons i (f i))))))]
    (lazy-seq (cons 2 (f 2)))))

(defn factor
  [n]
  (if (pos? n)
    (-> (reduce (fn [[m n] p]
                  (cond
                    (= n 1)
                    (reduced [m n])

                    (prime? n)
                    (reduced [(assoc m n 1) 1])

                    :else
                    (let [[n i] (loop [n n
                                       i 0]
                                  (if (zero? (rem n p))
                                    (recur (quot n p) (inc i))
                                    [n i]))]
                      (if (pos? i)
                        [(assoc m p i) n]
                        [m n]))))
                [{} n]
                primes)
        first)))
