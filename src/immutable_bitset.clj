(ns immutable-bitset
  (:import
    [Util BitSet]))

(deftype Chunk
  [^int generation
   ^BitSet bitset])

(defn- ^Chunk bitset-chunk [^long generation log2-chunk-size]
  (Chunk. generation (BitSet. (bit-shift-left 1 log2-chunk-size))))

(defn- bit-seq [^BitSet bitset ^long offset]
  (let [cnt (long (.Cardinality bitset))
        ^longs ary (long-array cnt)]
    (loop [ary-idx 0, set-idx 0]
      (when (< ary-idx cnt)
        (let [set-idx (.NextSetBit bitset set-idx)]
          (aset ary ary-idx (+ offset set-idx))
          (recur (inc ary-idx) (inc set-idx)))))
    (seq ary)))

(declare bitset ->persistent ->transient)

(defmacro ^:private assoc-bitset [x & {:as fields}]
  (let [type (-> &env (get x) .get_ClrType)
        field-names [:log2-chunk-size :generation :cnt :m :meta]]
    `(new ~type
       ~@(map
           (fn [field-name]
             (get fields field-name
               `(~(symbol (str "." (name field-name))) ~x)))
           field-names))))

(definline ^:private chunk-idx [n bit-shift]
  `(bit-shift-right ~n ~bit-shift))

(definline ^:private idx-within-chunk [n bit-shift]
  `(bit-and ~n (-> 1 (bit-shift-left ~bit-shift) dec)))

(definline ^:private dec-cnt [cnt]
  `(let [cnt# (unchecked-long ~cnt)]
     (if (== cnt# -1)
       cnt#
       (dec cnt#))))

(definline ^:private inc-cnt [cnt]
  `(let [cnt# (unchecked-long ~cnt)]
     (if (== cnt# -1)
       cnt#
       (inc cnt#))))

(defmacro ^:private compile-if [test then else]
  (if (eval test)
    then
    else))

(deftype PersistentBitSet
  [^byte log2-chunk-size
   ^int generation
   ^:volatile-mutable ^int cnt
   m
   meta]

  Object
  (GetHashCode [this]
    (if (zero? cnt)
      0
      (->> this
        (map #(bit-xor (long %) (unsigned-bit-shift-right (long %) 32)))
        (reduce #(+ (long %1) (long %2))))))
  (Equals [this x] (.equiv this x))

  clojure.lang.IHashEq
  (hasheq [this]
    (compile-if (resolve 'clojure.core/hash-unordered-coll)
      (hash-unordered-coll this)
      (.GetHashCode this)))

;; maybe this should be 
;  java.util.Set
;  (size [this] (count this))
;  (isEmpty [this] (zero? (count this)))
;  (iterator [this] (clojure.lang.SeqIterator. (seq this)))
;  (containsAll [this s] (every? #(contains? this %) s))

  System.Collections.IEnumerable
  (GetEnumerator [this] (clojure.lang.SeqEnumerator. this))

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [this meta] (assoc-bitset this :meta meta))

  clojure.lang.IEditableCollection
  (asTransient [this] (->transient this cnt))

  clojure.lang.Seqable
  (seq [_]
    (when-not (zero? cnt)
      (mapcat
        (fn [[slot ^Chunk v]]
          (bit-seq (.bitset v) (bit-shift-left (long slot) log2-chunk-size)))
        m)))

  clojure.lang.IFn
  (invoke [this idx]
    (when (contains? this idx)
      idx))

  clojure.lang.IPersistentSet
  (equiv [this x]
    (and
      (set? x)
      (= (count this) (count x))
      (every?
        #(contains? x %)
        (seq this))))
  (count [_]
    (when (== cnt -1)
      (set! cnt (->> m
                  vals
                  (map #(.Cardinality ^BitSet (.bitset ^Chunk %)))
                  (reduce +)
                  int)))
    cnt)
  (empty [_]
    (PersistentBitSet. log2-chunk-size 0 0 {} nil))
  (contains [_ n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (idx-within-chunk n log2-chunk-size)]
          (.Get ^BitSet (.bitset chunk) idx))
        false)))
  (disjoin [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (idx-within-chunk n log2-chunk-size)]
          (if (.Get ^BitSet (.bitset chunk) idx)
            (assoc-bitset this
              :cnt (dec-cnt cnt)
              :m (assoc m slot
                   (Chunk. generation
                     (doto ^BitSet (.Clone ^BitSet (.bitset chunk))
                       (.Set idx false)))))
            this))
        this)))
  (cons [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)
          idx (idx-within-chunk n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (if-not (.Get ^BitSet (.bitset chunk) idx)
          (assoc-bitset this
            :cnt (inc-cnt cnt)
            :m (assoc m slot
                 (Chunk. generation
                   (doto ^BitSet (.Clone ^BitSet (.bitset chunk))
                     (.Set idx true)))))
          this)
        (assoc-bitset this
          :cnt (inc-cnt cnt)
          :m (let [^Chunk chunk (bitset-chunk generation log2-chunk-size)]
               (.Set ^BitSet (.bitset chunk) idx true)
               (assoc m slot chunk)))))))

(deftype TransientBitSet
  [^byte log2-chunk-size
   ^int generation
   ^:volatile-mutable ^int cnt
   m
   meta]

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [this meta] (assoc-bitset this :meta meta))

  clojure.lang.ITransientSet
  (count [_]
    (when (== cnt -1)
      (set! cnt (->> m
                  vals
                  (map #(.Cardinality ^BitSet (.bitset ^Chunk %)))
                  (reduce +)
                  int)))
    cnt)
  (persistent [this] (->persistent this cnt))
  (contains [_ n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (idx-within-chunk n log2-chunk-size)]
          (.Get ^BitSet (.bitset chunk) idx))
        false)))
  (disjoin [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (idx-within-chunk n log2-chunk-size)]
          (if (.Get ^BitSet (.bitset chunk) idx)
            (if (== (.generation chunk) generation)
              (do
                (.Set ^BitSet (.bitset chunk) idx false)
                (assoc-bitset this :cnt (dec-cnt cnt)))
              (assoc-bitset this
                :cnt (dec-cnt cnt)
                :m (let [^BitSet bitset (.Clone ^BitSet (.bitset chunk))]
                     (.Set bitset idx false)
                     (assoc! m slot (Chunk. generation bitset)))))
            this))
        this)))
  (conj [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)
          idx (idx-within-chunk n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (if-not (.Get ^BitSet (.bitset chunk) idx)
          (if (== (.generation chunk) generation)
            (do
              (.Set ^BitSet (.bitset chunk) idx true)
              (assoc-bitset this :cnt (inc-cnt cnt)))
            (assoc-bitset this
              :cnt (inc-cnt cnt)
              :m (let [^BitSet bitset (.Clone ^BitSet (.bitset chunk))]
                   (.Set bitset idx true)
                   (assoc! m slot (Chunk. generation bitset)))))
          this)
        (assoc-bitset this
          :cnt (inc-cnt cnt)
          :m (let [^Chunk chunk (bitset-chunk generation log2-chunk-size)]
               (.Set ^BitSet (.bitset chunk) idx true)
               (assoc! m slot chunk)))))))

(defn- ->persistent [^TransientBitSet bitset ^long cnt]
  (PersistentBitSet.
    (.log2-chunk-size bitset)
    (.generation bitset)
    cnt
    (persistent! (.m bitset))
    (.meta bitset)))

(defn- ->transient [^PersistentBitSet bitset ^long cnt]
  (TransientBitSet.
    (.log2-chunk-size bitset)
    (inc (.generation bitset))
    cnt
    (transient (.m bitset))
    (.meta bitset)))

;;;

(defn sparse-bitset
  "Creates an immutable set which can only store integral values.  This should be used unless elements are densely
   clustered (each element has multiple elements within +/- 1000)."
  ([]
     ;; 128 bits per chunk
     (PersistentBitSet. 7 0 0 {} nil))
  ([s]
     (into (sparse-bitset) s)))

(defn dense-bitset
  "Creates an immutable set which can only store integral values.  This should be used only if elements are densely
   clustered (each element has multiple elements within +/- 1000)."
  ([]
     ;; 4096 bits per chunk
     (PersistentBitSet. 12 0 0 {} nil))
  ([s]
     (into (dense-bitset) s)))

;;;

(defn- merge-bit-op [bit-set-fn keys-fn ^PersistentBitSet a ^PersistentBitSet b]
  (assert (= (.log2-chunk-size a) (.log2-chunk-size b)))
  (let [log2-chunk-size (.log2-chunk-size a)
        generation (inc (long (max (.generation a) (.generation b))))
        m-a (.m a)
        m-b (.m b)
        ks (keys-fn m-a m-b)
        m (zipmap
            ks
            (map
              (fn [k]
                (let [^Chunk a (get m-a k)
                      ^Chunk b (get m-b k)]
                  (if (and a b)
                    (let [^Chunk chunk (Chunk. generation (.Clone ^BitSet (.bitset a)))
                          ^BitSet b-a (.bitset chunk)
                          ^BitSet b-b (.bitset b)]
                      (bit-set-fn b-a b-b)
                      chunk)
                    (or a b (throw (InvalidOperationException.))))))
              ks))]
    (PersistentBitSet.
      log2-chunk-size
      generation
      -1
      m
      nil)))

(defn union
  "Returns the union of two bitsets."
  [a b]
  (merge-bit-op
    #(.Or ^BitSet %1 %2)
    (fn [a b]
      (concat
        (keys a)
        (remove #(contains? a %) (keys b))))
    a
    b))

(defn intersection
  "Returns the intersection of two bitsets."
  [a b]
  (merge-bit-op
    #(.And ^BitSet %1 %2)
    (fn [a b]
      (filter #(contains? b %) (keys a)))
    a
    b))

(defn difference
  "Returns the difference between two bitsets."
  [a b]
  (merge-bit-op
    ;#(-> ^BitSet %1 (.Or %2) (.Xor %2))
    #(.AndNot ^BitSet %1 %2)
    (fn [a b] (keys a))
    a
    b))
