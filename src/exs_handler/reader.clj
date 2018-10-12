(ns exs-handler.reader
  (:require [exs-handler.format :as f])
  (:require [clojure.java.io :refer [file output-stream input-stream]])
  (:import [java.nio ByteBuffer ByteOrder])
  )

;; TODO: Multimethod reader
(defn unpack-with-format [dat fmt]
  (-> dat
      (merge (f/unpack (:chunkdata dat) fmt))
      (dissoc :chunkdata)
      )
  )
(defmulti unpack-chunk :sig)

(defn unpack-i [d o]
  (let [bb (ByteBuffer/allocate 4)
        _  (doto bb
             (.order (ByteOrder/LITTLE_ENDIAN))
             (.put (byte-array d) o 4)
             (.flip)
             )
        ]
    (.getInt bb)
    )
  )

(defn unpack-b [d o]
  (nth d o)
  )
(defn unpack-s [d o]
  (String. (byte-array (take-while (comp not zero?) (drop o d))) "UTF-8")
  )

(defn assoc-i [dat m k o] (assoc m k (unpack-i dat o)))
(defn assoc-b [dat m k o] (assoc m k (unpack-b dat o)))
(defn assoc-s [dat m k o] (assoc m k (unpack-s dat o)))


(defmethod unpack-chunk :zone [dat]
  (let [ai (partial assoc-i (:chunkdata  dat))
        ab (partial assoc-b (:chunkdata  dat))
        o  #(- % 84)
        ]
    (-> dat

        (ab :rootnote 1)
        (ab :finetune 2)
        (ab :pan 3)
        (ab :volume-adjust 4)
        (ab :scale 5)
        (ab :start-note 6)
        (ab :end-note 7)
        (ab :min-vel 9)
        (ab :max-vel 10)
        (ai :sample-start 12)
        (ai :sample-end 16)
        (ai :loop-start 20)
        (ai :loop-end 24)
        (ab :loop 33)
        ;; wonder what else is in here?!
        (ai :group (o 172))
        (ai :sample-index (o 176))
        (dissoc :chunkdata)
        ))
  )

(defmethod unpack-chunk :sample [dat]
  (let [ai (partial assoc-i (:chunkdata  dat))
        ab (partial assoc-b (:chunkdata  dat))
        ast (partial assoc-s (:chunkdata  dat))
        o  #(- % 84)
        ]
    (-> dat
        (ai :length 4)
        (ai :rate 8)
        (ai :bitdepth 12)
        (ast :path 80)
        (ast :file (+ 80 256))
        ;; there's the path at 80 and sample name at 336 which is 80 + 256 so...
        (dissoc :chunkdata)
        ))
  )

(defmethod unpack-chunk :header [dat] (unpack-with-format dat f/chunk-type-header))

(defmethod unpack-chunk :default [dat]
  (-> dat
      (assoc :error "Parser not implemented")
      (assoc :data-as-char (map #(if (> % 0) (char %) \.) (:chunkdata dat)))
      ;;(dissoc :chunkdata)
      )
  )

(defn raw-data [fn]
  (with-open [in (input-stream (file fn))]
    (let [buf (byte-array (* 1024 1024))
          n (.read in buf)
          ]
      {:unparsed  (take n  buf)
       :chunks []})
    )
  )

(defn add-chunk [d c] (update d :chunks conj c))

(defn unpack-common-84 [bts] (f/unpack bts f/common-84-header) )

(defn parse-chunk [d]
  (let [sigsz (unpack-common-84 (:unparsed d))
        chlen (+ 84 (:size sigsz))
        hdr (take chlen (:unparsed d))
        chdat (drop 84 hdr)
        unp (drop chlen (:unparsed d))

        newchunk (-> sigsz
                     (assoc :chunkdata chdat))
        ]
    (-> d
        (add-chunk (unpack-chunk newchunk))
        (assoc :unparsed unp)))
  )


(defn read-file [fn]
  (let [dfile  (raw-data fn)]
    (loop [df dfile]
      (if (zero? (count (:unparsed df)))
        (:chunks  df)
        (recur (parse-chunk df))
        )
      )
    ))


(comment loop [d (->>  (read-file "/Users/paul/Music/Audio Music Apps/Sampler Instruments/TryLinn2.exs")
                       (filter #(= (:sig %) :sample))
                       first
                       :chunkdata
                       ;; (filter #(> % 0))
                       (map #(if (> % 0) (char %) 0))

                       )
               i 0
               ]
         (if (empty? d) 0
             (do
               (println i (first d))
               (recur (rest d) (inc i))
               ))
         )


(->>  (read-file "/Users/paul/Music/Audio Music Apps/Sampler Instruments/SingleSample.exs")

      )

