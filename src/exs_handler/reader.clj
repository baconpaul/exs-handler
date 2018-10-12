(ns exs-handler.reader
  (:require [exs-handler.format :as f])
  (:require [clojure.java.io :refer [file output-stream input-stream]])
  (:import [java.nio ByteBuffer ByteOrder])
  )

(defn unpack-with-format [dat fmt]
  (-> dat
      (merge (f/unpack (:chunkdata dat) fmt))
      (dissoc :chunkdata)
      )
  )

(defn unpack-common-84 [bts] (f/unpack bts f/common-84-header) )

(defmulti unpack-chunk :sig)

(defmethod unpack-chunk :zone [dat] (unpack-with-format dat f/chunk-type-zone))
(defmethod unpack-chunk :sample [dat] (unpack-with-format dat f/chunk-type-sample))
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


(def dfb  "/Library/Application Support/Logic/Sampler Instruments/02 Bass/03 Synth Bass/Sine Sub Bass.exs")
(def tl2 "/Users/paul/Music/Audio Music Apps/Sampler Instruments/TryLinn2.exs")
(->>  (read-file tl2)
      (filter #(= (:sig %) :zone))
      (map :sample-index)
      )

