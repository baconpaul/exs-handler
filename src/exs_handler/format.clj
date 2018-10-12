(ns exs-handler.format
  (:require [clojure.java.io :refer [file output-stream input-stream]])
  (:import [java.nio ByteBuffer ByteOrder])
  )

(def common-84-header
  [
   [ :sig-value :int ]
   [ :size :int ]
   [ :id :int ]
   [ :unknown-hdr :int ]
   [ :tbos :literal (byte-array (map int  [ \T \B \O \S ]))]
   [ :name :string 64 ]

   [ :sig-lsb :calc (fn [m] (bit-and  (:sig-value m) 0x0FFFFFFF))]
   [ :sig-msb :calc (fn [m] (bit-and  (:sig-value m) 0xF0000000))]
   [ :sig     :calc (fn [m]
                      (condp = (:sig-lsb m)
                        0x00000101 :header
                        0x01000101 :zone
                        0x02000101 :group
                        0x03000101 :sample
                        0x04000101 :param

                        
                        {:unknown-type (format "0x%08x" (:sig-lsb m))  }
                        )
                      )]
   ]
  )

(def chunk-type-header
  "The header chunk contains 80 bytes. The format is

  an int of some form
  zone count
  group count
  sample count
  some extra bytes
  " 
  [
   [nil :int]
   [:zone-count :int]
   [:group-count :int]
   [:sample-count :int]
   ])

(def chunk-type-sample
  "The sample chunk contains data about the sample (length and so on) and then the file and path name"
  [
   [:unknown-first-int :int] ;; This appears to be the number 44 or 4096. Who knows why?
   [:length            :int]
   [:rate              :int]
   [:bitdepth          :int]
   [:unknown-region    :slurp-up-to 80]
   [:path              :string 256]
   [:file              :string 256]
   ]
  )

(def chunk-type-zone
  "The zone format is full of little bits"
  [
   [:open-bitmask :byte]
   [:rootnote :byte]
   [:finetune :byte]
   [:pan :byte]
   [:volume-adjust :byte]
   [:scale :byte]
   [:start-note :byte]
   [:end-note :byte]
   [nil :byte]
   [:min-vel :byte]
   [:max-vel :byte]
   [nil :byte]
   [:sample-start :int]
   [:sample-end :int]
   [:loop-start :int]
   [:loop-end :int]
   [nil :slurp-up-to 33]
   [:loop :byte]
   [nil :slurp-up-to 88]
   [:group :int]
   [:sample-index :int]
   ]
  )


(defn format-byte-size [format]
  (loop [f format
         sz 0
         ]

    (if (empty? f)
      sz
      (let [ff (first f)
            rf (rest f)
            fsz (condp = (second ff) ;; which is the type
                  :byte      1
                  :int       4
                  :literal   (count (last ff))
                  :string    (last ff)

                  :slurp-up-to (- (last ff) sz)
                  
                  :calc      0
                  )
            ]
        (recur rf (+ sz fsz))
        )
      )
    )
  )

(defn unpack [bytes format]
  (let [fsz (format-byte-size format)]
    (when (< (count bytes) fsz)
      (throw (Exception. (str "We require at least " fsz " bytes and we got " (count bytes))))
      )
    (let [bb (ByteBuffer/allocate fsz)
          _  (doto bb
               (.order (ByteOrder/LITTLE_ENDIAN))
               (.put (byte-array bytes) 0 fsz)
               (.flip)
               )
          ]
      (loop [f format
             r {}
             pos 0
             ]
        (if (empty? f) r
            (let [ff (first f)
                  rf (rest f)
                  name (first ff)
                  type (second ff)

                  value
                  (condp = type
                    :int  (.getInt bb)
                    :byte (.get bb)
                    :string (let [len (last ff)
                                  ba  (byte-array len)
                                  ]
                              (.get bb ba)
                              (String. (byte-array (take-while (comp not zero?) ba)) "UTF-8")
                              )
                    :literal (let [lit (last ff)
                                   lsz (count lit)
                                   ba  (byte-array lsz)
                                   ]
                               (.get bb ba)
                               (when (not= (seq ba) (seq lit))
                                 (throw (Exception. (str  "Non-equivalent literals " (seq ba) (seq lit))))
                                 )
                               lit
                               )

                    :slurp-up-to (let [ss (last ff)
                                       ba (byte-array (- ss pos))
                                       ]
                                   (.get bb ba)
                                   ba
                                   )
                    :calc   (let [cfn (last ff)]
                              (cfn r)
                              )
                    
                    )
                  npos
                  (condp = type
                    :int (+ pos 4)
                    :byte (+ pos 1)
                    :string (+ pos (last ff))
                    :literal (+ pos (count (last ff)))
                    :slurp-up-to (last ff)
                    pos
                    )
                  
                  ]
              (recur rf (conj (if (nil? name) r (assoc r name value))) npos)
              ))
        )
      ))
  )

