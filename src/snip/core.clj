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
