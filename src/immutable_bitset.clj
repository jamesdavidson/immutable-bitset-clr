(ns immutable-bitset)

;; MVP: Clone Cardinality Get Set(idx,val) Or And AndNot
;; which implies Set(idx) and Clear(idx) as well as Ensure(lastElt)

;; translated from BitSet.cs which is Copyright © 1998, 1999, 2000, 2001, 2004, 2005  Free Software Foundation, Inc.  (see file for more information)

(definterface MutableBitSet
  (^Void And [bs])
  (^Void AndNot [bs])
  (^int Cardinality [])
  (^Void Clear ())
  (^Void Clear [^int pos])
  (^Void Clear [^int from, ^int to])
  (^Boolean Equals [obj])
  (^Void XOr [bs])
  (^Void Flip [^int from, ^int to])
  (^Boolean Get [^int pos])
  (^BitSet Get [^int from, ^int to])
  (^Boolean Intersects [set])
  (^Boolean IsEmpty [])
  (^int Length [])
  (^int NextClearBit [^int from])
  (^int NextSetBit [^int from])
  (^Void Or [bs])
  (^Void Set [^int pos])
  (^Void Set [^int pos ^Boolean value])
  ;    [^int from, ^int to]
  ;    [^int from, ^int to, ^bool value])
  (^int Size [])
  (^Void XOr [bs])
  (^Void Ensure [lastElt])
  (^Boolean ContainsAll [other]))

(deftype BitSet
  [^:volatile-mutable ^longs bits]

  Object
  (ToString [this]
    (let [n     (.Length bits)
          r     (new StringBuilder "{")
          first (volatile! true)]
      (with-local-vars [first true]
        (loop [i 0]
          (when (< i n)
                (let [word (aget bits i)]
                  (when-not (zero? word)
                            (dotimes [j 64]
                              (let [bit (bit-shift-left 1 j)]
                                (when-not (zero? (bit-and word bit))
                                          (if (var-get first)
                                            (var-set first false)
                                            (.Append r ", "))
                                          (.Append r (+ j (* 64 i))))))))
                (recur (inc i)))))
      (.Append r "}")
      (.ToString r)))

  ;        /// <summary>
  ;        /// Returns the string representation of this bit set.  This
  ;        /// consists of a comma separated list of the integers in this set
  ;        /// surrounded by curly braces.  There is a space after each comma.
  ;        /// A sample string is thus "{1, 3, 53}".
  ;        /// </summary>
  ;        /// <returns>the string representation.</returns>
  ;        public override string ToString()
  ;        {
  ;            var r = new StringBuilder("{");
  ;            bool first = true;
  ;            for (int i = 0; i < bits.Length; ++i)
  ;            {
  ;                long bit = 1;
  ;                long word = bits[i];
  ;                if (word == 0)
  ;                    continue;
  ;                for (int j = 0; j < 64; ++j)
  ;                {
  ;                    if ((word & bit) != 0)
  ;                    {
  ;                        if (!first)
  ;                            r.Append(", ");
  ;                        r.Append(64 * i + j);
  ;                        first = false;
  ;                    }
  ;                    bit <<= 1;
  ;                }
  ;            }
  ;
  ;            return r.Append("}").ToString();
  ;        }

  ;        /// <summary>
  ;        /// Returns true if the <code>obj</code> is a bit set that contains
  ;        /// exactly the same elements as this bit set, otherwise false.
  ;        /// </summary>
  ;        /// <param name="obj">the object to compare to</param>
  ;        /// <returns>true if obj equals this bit set</returns>
  ;        public override bool Equals(object obj)
  ;        {
  ;            if (!(obj.GetType() == typeof(BitSet)))
  ;	            return false;
  ;            BitSet bs = (BitSet) obj;
  ;            int max = Math.Min(bits.Length, bs.bits.Length);
  ;            int i;
  ;            for (i = 0; i < max; ++i)
  ;	        if (bits[i] != bs.bits[i])
  ;		        return false;
  ;            // If one is larger, check to make sure all extra bits are 0.
  ;            for (int j = i; j < bits.Length; ++j)
  ;	            if (bits[j] != 0)
  ;		            return false;
  ;            for (int j = i; j < bs.bits.Length; ++j)
  ;	            if (bs.bits[j] != 0)
  ;		            return false;
  ;            return true;
  ;        }

  ;        /// <summary>
  ;        /// Returns a hash code value for this bit set.  The hash code of
  ;        /// two bit sets containing the same integers is identical.  The algorithm
  ;        /// used to compute it is as follows:
  ;        /// 
  ;        /// Suppose the bits in the BitSet were to be stored in an array of
  ;        /// long integers called <code>bits</code>, in such a manner that
  ;        /// bit <code>k</code> is set in the BitSet (for non-negative values
  ;        /// of <code>k</code>) if and only if
  ;        /// 
  ;        /// <code>((k/64) &lt; bits.length)
  ;        /// && ((bits[k/64] & (1L &lt;&lt; (bit % 64))) != 0)
  ;        /// </code>
  ;        /// 
  ;        /// Then the following definition of the GetHashCode method
  ;        /// would be a correct implementation of the actual algorithm:
  ;        /// 
  ;        /// <pre>public override int GetHashCode()
  ;        /// {
  ;        ///   long h = 1234;
  ;        ///   for (int i = bits.length-1; i &gt;= 0; i--)
  ;        ///   {
  ;        ///     h ^= bits[i] * (i + 1);
  ;        ///   }
  ;        ///   
  ;        ///   return (int)((h >> 32) ^ h);
  ;        /// }</pre>
  ;        /// 
  ;        /// Note that the hash code values changes, if the set is changed.
  ;        /// </summary>
  ;        /// <returns>the hash code value for this bit set.</returns>
  ;        public override int GetHashCode()
  ;        {
  ;            long h = 1234;
  ;            for (int i = bits.Length; i > 0; )
  ;            h ^= i * bits[--i];
  ;            return (int) ((h >> 32) ^ h);
  ;        }
  ;

  ICloneable
  (Clone [this]
    (let [bits-cloned (.Clone bits)]
      (new BitSet bits-cloned)))

  ;        /// <summary>
  ;        /// Create a clone of this bit set, that is an instance of the same
  ;        /// class and contains the same elements.  But it doesn't change when
  ;        /// this bit set changes.
  ;        /// </summary>
  ;        /// <returns>the clone of this object.</returns>
  ;        public object Clone()
  ;        {
  ;            try
  ;            {
  ;                var bs = new BitSet();
  ;                bs.bits = (long[])this.bits.Clone();
  ;                return bs;
  ;            }
  ;            catch
  ;            {
  ;                // Impossible to get here.
  ;                return null;
  ;            }
  ;        }

  MutableBitSet
  (And [this bs]
    (let [n (.Length bits)
          m (min n (.Length ^longs (.bits ^BitSet bs)))]
      (loop [i 0]
        (when (< i n)
              (if (< i m)
                (aset bits i (bit-and (aget bits i) (aget ^longs (.bits ^BitSet bs) i)))
                (aset bits i 0))
              (recur (inc i))))))

  ;        public void And(BitSet bs)
  ;        {
  ;            int max = Math.Min(bits.Length, bs.bits.Length);
  ;            int i;
  ;            for (i = 0; i < max; ++i)
  ;                bits[i] &= bs.bits[i];
  ;            while (i < bits.Length)
  ;                bits[i++] = 0;
  ;        }

  (AndNot [this bs]
    (let [n (.Length bits)
          m (min n (.Length ^longs (.bits ^BitSet bs)))]
      (loop [i 0]
        (when (< i m)
              (aset bits i (bit-and (aget bits i) (bit-not (aget ^longs (.bits ^BitSet bs) i))))
              (recur (inc i))))))

  ;        public void AndNot(BitSet bs)
  ;        {
  ;            int i = Math.Min(bits.Length, bs.bits.Length);
  ;            while (--i >= 0)
  ;                bits[i] &= ~bs.bits[i];
  ;        }

  (Cardinality [this]
    (let [n (.Length bits)]
      (loop [i   0
             acc 0]
        (if (< i n)
          (recur (inc i) (+ acc (Int64/PopCount (aget bits i))))
          acc))))

  ;        public int Cardinality()
  ;        {
  ;            uint card = 0;
  ;            for (int i = bits.Length - 1; i >= 0; i--)
  ;            {
  ;                long a = bits[i];
  ;                // Take care of common cases.
  ;                if (a == 0)
  ;                    continue;
  ;                if (a == -1)
  ;                {
  ;                    card += 64;
  ;                    continue;
  ;                }
  ;
  ;                // Successively collapse alternating bit groups into a sum.
  ;                a = ((a >> 1) & 0x5555555555555555L) + (a & 0x5555555555555555L);
  ;                a = ((a >> 2) & 0x3333333333333333L) + (a & 0x3333333333333333L);
  ;                uint b = (uint)((a >> 32) + a);
  ;                b = ((b >> 4) & 0x0f0f0f0f) + (b & 0x0f0f0f0f);
  ;                b = ((b >> 8) & 0x00ff00ff) + (b & 0x00ff00ff);
  ;                card += ((b >> 16) & 0x0000ffff) + (b & 0x0000ffff);
  ;            }
  ;            return (int)card;
  ;        }

  (Clear [this]
    (let [n (.Length bits)]
      (loop [i 0]
        (when (< i n)
              (aset bits i 0)
              (recur (inc i))))))

  ;        public void Clear()
  ;        {
  ;            Array.Fill(bits, 0);
  ;        }

  (Clear [this pos]
    (let [n      (.Length bits)
          offset (bit-shift-right pos 6)]
      (.Ensure this offset)
      (aset bits offset (bit-and (aget bits offset) (bit-not (bit-shift-left 1 pos))))))

  ;        /// <summary>
  ;        /// Removes the integer <code>pos</code> from this set. That is
  ;        /// the corresponding bit is cleared.  If the index is not in the set,
  ;        /// this method does nothing.
  ;        /// </summary>
  ;        /// <param name="pos">a non-negative integer</param>
  ;        public void Clear(int pos)
  ;        {
  ;            int offset = pos >> 6;
  ;            Ensure(offset);
  ;            bits[offset] &= ~(1L << pos);
  ;        }

  ;        /// <summary>
  ;        /// Sets the bits between from (inclusive) and to (exclusive) to false.
  ;        /// </summary>
  ;        /// <param name="from">the start range (inclusive)</param>
  ;        /// <param name="to">the end range (exclusive)</param>
  ;        public void Clear(int from, int to)
  ;        {
  ;            if (from < 0 || from > to)
  ;	            throw new ArgumentOutOfRangeException();
  ;            if (from == to)
  ;	            return;
  ;            uint lo_offset = (uint)from >> 6;
  ;            uint hi_offset = (uint)to >> 6;
  ;            Ensure((int)hi_offset);
  ;            if (lo_offset == hi_offset)
  ;            {
  ;	            bits[hi_offset] &= ((1L << from) - 1) | (-1L << to);
  ;	            return;
  ;            }
  ;
  ;            bits[lo_offset] &= (1L << from) - 1;
  ;            bits[hi_offset] &= -1L << to;
  ;            for (int i = (int)lo_offset + 1; i < hi_offset; i++)
  ;                bits[i] = 0;
  ;        }

  ;        /// <summary>
  ;        /// Sets the bit at the index to the opposite value.
  ;        /// </summary>
  ;        /// <param name="index">the index of the bit</param>
  ;        public void Flip(int index)
  ;        {
  ;            int offset = index >> 6;
  ;            Ensure(offset);
  ;            bits[offset] ^= 1L << index;
  ;        }
  ;
  ;        /// <summary>
  ;        /// Sets a range of bits to the opposite value.
  ;        /// </summary>
  ;        /// <param name="from">the low index (inclusive)</param>
  ;        /// <param name="to">the high index (exclusive)</param>
  ;        public void Flip(int from, int to)
  ;        {
  ;            if (from < 0 || from > to)
  ;                throw new ArgumentOutOfRangeException();
  ;            if (from == to)
  ;                return;
  ;            uint lo_offset = (uint)from >> 6;
  ;            uint hi_offset = (uint)to >> 6;
  ;            Ensure((int)hi_offset);
  ;            if (lo_offset == hi_offset)
  ;            {
  ;                bits[hi_offset] ^= (-1L << from) & ((1L << to) - 1);
  ;                return;
  ;            }
  ;
  ;            bits[lo_offset] ^= -1L << from;
  ;            bits[hi_offset] ^= (1L << to) - 1;
  ;            for (int i = (int)lo_offset + 1; i < hi_offset; i++)
  ;                bits[i] ^= -1;
  ;        }

  (Get [this pos]
    (let [n      (.Length bits)
          offset (bit-shift-right pos 6)]
      (if-not (<= offset n)
              false
              (not (zero? (bit-and (aget bits offset) (bit-shift-left 1 pos)))))))

  ;        /// <summary>
  ;        /// Returns true if the integer <code>bitIndex</code> is in this bit
  ;        /// set, otherwise false.
  ;        /// </summary>
  ;        /// <param name="pos">a non-negative integer</param>
  ;        /// <returns>the value of the bit at the specified position</returns>
  ;        public Boolean Get(int pos)
  ;        {
  ;            int offset = pos >> 6;
  ;            if (offset >= bits.Length)
  ;                return false;
  ;            return (bits[offset] & (1L << pos)) != 0;
  ;        }
  ;
  ;        /// <summary>
  ;        /// Returns a new <code>BitSet</code> composed of a range of bits from
  ;        /// this one.
  ;        /// </summary>
  ;        /// <param name="from">the low index (inclusive)</param>
  ;        /// <param name="to">the high index (exclusive)</param>
  ;        /// <returns></returns>
  ;        public BitSet Get(int from, int to)
  ;        {
  ;            if (from < 0 || from > to)
  ;	            throw new ArgumentOutOfRangeException();
  ;            BitSet bs = new BitSet(to - from);
  ;            uint lo_offset = (uint)from >> 6;
  ;            if (lo_offset >= bits.Length || to == from)
  ;	            return bs;
  ;
  ;            int lo_bit = from & LONG_MASK;
  ;            uint hi_offset = (uint)to >> 6;
  ;            if (lo_bit == 0)
  ;            {
  ;	            uint len = Math.Min(hi_offset - lo_offset + 1, (uint)bits.Length - lo_offset);
  ;                Array.Copy(bits, lo_offset, bs.bits, 0, len);
  ;	            if (hi_offset < bits.Length)
  ;	            bs.bits[hi_offset - lo_offset] &= (1L << to) - 1;
  ;	            return bs;
  ;            }
  ;
  ;            uint len2 = Math.Min(hi_offset, (uint)bits.Length - 1);
  ;            int reverse = 64 - lo_bit;
  ;            int i;
  ;            for (i = 0; lo_offset < len2; lo_offset++, i++)
  ;                bs.bits[i] = ((bits[lo_offset] >> lo_bit) | (bits[lo_offset + 1] << reverse));
  ;            if ((to & LONG_MASK) > lo_bit)
  ;	            bs.bits[i++] = bits[lo_offset] >> lo_bit;
  ;            if (hi_offset < bits.Length)
  ;	            bs.bits[i - 1] &= (1L << (to - from)) - 1;
  ;            return bs;
  ;        }

  ;        /// <summary>
  ;        /// Returns true if the specified BitSet and this one share at least one
  ;        /// common true bit.
  ;        /// </summary>
  ;        /// <param name="set">the set to check for intersection</param>
  ;        /// <returns>true if the sets intersect</returns>
  ;        public bool Intersects(BitSet set)
  ;        {
  ;            int i = Math.Min(bits.Length, set.bits.Length);
  ;            while (--i >= 0)
  ;            {
  ;                if ((bits[i] & set.bits[i]) != 0)
  ;                    return true;
  ;            }
  ;            return false;
  ;        }

  (IsEmpty [this]
    (let [n (.Length bits)]
      (println n)
      (loop [i   0
             acc true]
        (if (< i n)
          (recur (inc i) (and acc (zero? (aget bits i))))
          acc))))

  ;        /// <summary>
  ;        /// Returns true if this set contains no true bits.
  ;        /// </summary>
  ;        /// <returns>true if all bits are false</returns>
  ;        public bool IsEmpty()
  ;        {
  ;            for (int i = bits.Length - 1; i >= 0; i--)
  ;            {
  ;                if (bits[i] != 0)
  ;                    return false;
  ;            }
  ;            return true;
  ;        }
  ;
  ;        /// <summary>
  ;        /// Gets the logical number of bits actually used by this bit
  ;        /// set.  It returns the index of the highest set bit plus one.
  ;        /// Note that this method doesn't return the number of set bits.
  ;        /// 
  ;        /// Returns the index of the highest set bit plus one.
  ;        /// </summary>
  ;        public int Length
  ;        {
  ;            get
  ;            {
  ;                // Set i to highest index that contains a non-zero value.
  ;                int i;
  ;                for (i = bits.Length - 1; i >= 0 && bits[i] == 0; --i){}
  ;
  ;                // if i < 0 all bits are cleared.
  ;                if (i < 0)
  ;                    return 0;
  ;
  ;                // Now determine the exact length.
  ;                long b = bits[i];
  ;                int len = (i + 1) * 64;
  ;                // b >= 0 checks if the highest bit is zero.
  ;                while (b >= 0)
  ;                {
  ;                    --len;
  ;                    b <<= 1;
  ;                }
  ;
  ;                return len;
  ;            }
  ;        }
  ;
  ;        /// <summary>
  ;        /// Returns the index of the next false bit, from the specified bit
  ;        /// (inclusive).
  ;        /// </summary>
  ;        /// <param name="from">the start location</param>
  ;        /// <returns>the first false bit</returns>
  ;        public int NextClearBit(int from)
  ;        {
  ;            int offset = from >> 6;
  ;            long mask = 1L << from;
  ;            while (offset < bits.Length)
  ;            {
  ;                long h = bits[offset];
  ;                do
  ;                {
  ;                    if ((h & mask) == 0)
  ;                        return from;
  ;                    mask <<= 1;
  ;                    from++;
  ;                }
  ;                while (mask != 0);
  ;
  ;                mask = 1;
  ;                offset++;
  ;            }
  ;
  ;            return from;
  ;        }

  (NextSetBit [this pos]
    (let [s (.Size this)]
      (loop [i pos]
        (if (< i s)
          (if (.Get this i)   ;; TODO optimise
            i
            (recur (inc i)))
          -1))))

  ;        /// <summary>
  ;        /// Returns the index of the next true bit, from the specified bit
  ;        /// (inclusive). If there is none, -1 is returned. You can iterate over
  ;        /// all true bits with this loop:<br>
  ;        /// 
  ;        /// <pre>for (int i = bs.nextSetBit(0); i &gt;= 0; i = bs.nextSetBit(i + 1))
  ;        /// {
  ;        ///   // operate on i here
  ;        /// }
  ;        /// </pre>
  ;        /// </summary>
  ;        /// <param name="from">the start location</param>
  ;        /// <returns>the first true bit, or -1</returns>
  ;        public int NextSetBit(int from)
  ;        {
  ;            int offset = from >> 6;
  ;            long mask = 1L << from;
  ;            while (offset < bits.Length)
  ;            {
  ;                long h = bits[offset];
  ;                do
  ;                {
  ;                    if ((h & mask) != 0)
  ;                        return from;
  ;                    mask <<= 1;
  ;                    from++;
  ;                }
  ;                while (mask != 0);
  ;
  ;                mask = 1;
  ;                offset++;
  ;            }
  ;
  ;            return -1;
  ;        }

  (Or [this bs]
    (let [n (.Length ^longs (.bits ^BitSet bs))]
      (.Ensure this (dec n))
      (loop [i 0]
        (when (< i n)
              (aset bits i (bit-or (aget bits i) (aget ^longs (.bits ^BitSet bs) i)))
              (recur (inc i))))))

  ;        /// <summary>
  ;        /// Performs the logical OR operation on this bit set and the
  ;        /// given <code>set</code>.  This means it builds the union
  ;        /// of the two sets.  The result is stored into this bit set, which
  ;        /// grows as necessary.
  ;        /// </summary>
  ;        /// <param name="bs">the second bit set</param>
  ;        public void Or(BitSet bs)
  ;        {
  ;            Ensure(bs.bits.Length - 1);
  ;            for (int i = bs.bits.Length - 1; i >= 0; i--)
  ;                bits[i] |= bs.bits[i];
  ;        }

  (Set [this pos]
    (let [n      (.Length bits)
          offset (bit-shift-right pos 6)]
      (.Ensure this offset)
      (aset bits offset (bit-or (aget bits offset) (bit-shift-left 1 pos)))))

  (Set [this pos value]
    (if value
      (.Set this pos)
      (.Clear this pos)))

  ;        /// <summary>
  ;        /// Add the integer <code>bitIndex</code> to this set.  That is
  ;        /// the corresponding bit is set to true.  If the index was already in
  ;        /// the set, this method does nothing.  The size of this structure
  ;        /// is automatically increased as necessary.
  ;        /// </summary>
  ;        /// <param name="pos">a non-negative integer.</param>
  ;        public void Set(int pos)
  ;        {
  ;            int offset = pos >> 6;
  ;            Ensure(offset);
  ;            bits[offset] |= 1L << pos;
  ;        }
  ;        /// <summary>
  ;        /// Sets the bit at the given index to the specified value. The size of
  ;        /// this structure is automatically increased as necessary.
  ;        /// </summary>
  ;        /// <param name="index">the position to set</param>
  ;        /// <param name="value">the value to set it to</param>
  ;        public void Set(int index, bool value)
  ;        {
  ;            if (value)
  ;                this.Set(index);
  ;            else
  ;                this.Clear(index);
  ;        }
  ;
  ;        /// <summary>
  ;        /// Sets the bits between from (inclusive) and to (exclusive) to true.
  ;        /// </summary>
  ;        /// <param name="from">the start range (inclusive)</param>
  ;        /// <param name="to">the end range (exclusive)</param>
  ;        public void Set(int from, int to)
  ;        {
  ;            if (from < 0 || from > to)
  ;                throw new ArgumentOutOfRangeException();
  ;            if (from == to)
  ;                return;
  ;            uint lo_offset = (uint)from >> 6;
  ;            uint hi_offset = (uint)to >> 6;
  ;            Ensure((int)hi_offset);
  ;            if (lo_offset == hi_offset)
  ;            {
  ;                bits[hi_offset] |= (-1L << from) & ((1L << to) - 1);
  ;                return;
  ;            }
  ;
  ;            bits[lo_offset] |= -1L << from;
  ;            bits[hi_offset] |= (1L << to) - 1;
  ;            for (int i = (int)lo_offset + 1; i < hi_offset; i++)
  ;                bits[i] = -1;
  ;        }
  ;
  ;        /// <summary>
  ;        /// Sets the bits between from (inclusive) and to (exclusive) to the
  ;        /// specified value.
  ;        /// </summary>
  ;        /// <param name="from">the start range (inclusive)</param>
  ;        /// <param name="to">the end range (exclusive)</param>
  ;        /// <param name="value">the value to set it to</param>
  ;        public void Set(int from, int to, bool value)
  ;        {
  ;            if (value)
  ;                this.Set(from, to);
  ;            else
  ;                this.Clear(from, to);
  ;        }

  (Size [this]
    (let [n (.Length bits)]
      (* n 64)))

  ;        /// <summary>
  ;        /// Returns the number of bits actually used by this bit set.  Note
  ;        /// that this method doesn't return the number of set bits, and that
  ;        /// future requests for larger bits will make this automatically grow.
  ;        /// 
  ;        /// Returns the number of bits currently used.
  ;        /// </summary>
  ;        public int Size
  ;        {
  ;            get
  ;            {
  ;                return bits.Length * 64;
  ;            }
  ;        }

  ;        /// <summary>
  ;        /// Performs the logical XOR operation on this bit set and the
  ;        /// given <code>set</code>.  This means it builds the symmetric
  ;        /// remainder of the two sets (the elements that are in one set,
  ;        /// but not in the other).  The result is stored into this bit set,
  ;        /// which grows as necessary.
  ;        /// </summary>
  ;        /// <param name="bs">the second bit set</param>
  ;        public void XOr(BitSet bs)
  ;        {
  ;            Ensure(bs.bits.Length - 1);
  ;            for (int i = bs.bits.Length - 1; i >= 0; i--)
  ;                bits[i] ^= bs.bits[i];
  ;        }

  ;        /// <summary>
  ;        /// Make sure the vector is big enough.
  ;        /// </summary>
  ;        /// <param name="lastElt">the size needed for the bits array</param>
  ;        private void Ensure(int lastElt)
  ;        {
  ;            if (lastElt >= bits.Length)
  ;            {
  ;                long[] nd = new long[lastElt + 1];
  ;                Array.Copy(bits, 0, nd, 0, bits.Length);
  ;                bits = nd;
  ;            }
  ;        }

  ;        // This is used by EnumSet for efficiency.
  ;        public bool ContainsAll(BitSet other)
  ;        {
  ;            for (int i = other.bits.Length - 1; i >= 0; i--)
  ;            {
  ;                if ((bits[i] & other.bits[i]) != other.bits[i])
  ;                    return false;
  ;            }
  ;
  ;            return true;
  ;        }

  (Ensure [this offset]
    (let [n (.Length bits)]
      (when (<= n offset)
            (let [nd (long-array (inc offset))]
              (Array/Copy bits 0 nd 0 n)
              (set! bits nd)))))

  ;        /// <summary>
  ;        /// Make sure the vector is big enough.
  ;        /// </summary>
  ;        /// <param name="lastElt">the size needed for the bits array</param>
  ;        private void Ensure(int lastElt)
  ;        {
  ;            if (lastElt >= bits.Length)
  ;            {
  ;                long[] nd = new long[lastElt + 1];
  ;                Array.Copy(bits, 0, nd, 0, bits.Length);
  ;                bits = nd;
  ;            }
  ;        }

  )

(defn- make-mutable-bitset
  ([] (make-mutable-bitset 64))
  ([nbits]
   {:pre [(nat-int? nbits)]}
   (let [LONG_MASK 0x3f
         length    (uint (bit-shift-right nbits 6))
         arr       (if (zero? (bit-and nbits LONG_MASK))
                     (long-array length)
                     (long-array (inc length)))]
     (new BitSet arr))))

(comment

 (def a (make-mutable-bitset))
 (def b (make-mutable-bitset))

 (.Clear a)
 (.And a b)
 (.AndNot a b)

 ;(str a)

 (set! (.bits a) (long-array [3]))

 (.ToString a)
 (.Cardinality a)
 (.Get a 0)
 (.Get a 1)
 (.Get a 2)
 (.Get a 3)

 (.IsEmpty a)
 (.IsEmpty b)

 (.bits a)

 (.Ensure a 2)

 (.Set b 66 true)
 (.Clear a 66)

 (.Or a b)

 (pprint a))

(deftype Chunk
  [^int generation
   ^BitSet bitset])

(defn- ^Chunk bitset-chunk [^long generation log2-chunk-size]
  (Chunk. generation (make-mutable-bitset (bit-shift-left 1 log2-chunk-size))))

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

  System.Collections.ICollection
  (CopyTo [this arr idx] (throw (InvalidOperationException.)))

  (get_Count [this] cnt)
  (get_IsSynchronized [_] true)
  (get_SyncRoot [this] this)

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
