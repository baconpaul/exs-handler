(ns exs-handler.format
  (:require [clojure.java.io :refer [file output-stream input-stream]])
  (:import [java.nio ByteBuffer ByteOrder])
  )

(def common-84-header
  [
   [ :sig-value :int ]
   [ :size :int ]
   [ :id :int ]
   [ :unknown :int ]
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
                    :calc   (let [cfn (last ff)]
                              (cfn r)
                              )
                    
                    )
                  
                  ]
              (recur rf (conj (if (nil? name) r (assoc r name value))))
              ))
        )
      ))
  )


(defn tmp-raw-data [fn]
  (with-open [in (input-stream (file fn))]
    (let [buf (byte-array (* 1024 1024))
          n (.read in buf)
          ]
      {:unparsed  (take n  buf)
       :chunks []})
    )
  )

(->  (tmp-raw-data "/Users/paul/Music/Audio Music Apps/Sampler Instruments/Linn Drum Machine.exs")
     :unparsed
     (unpack common-84-header)
     )

