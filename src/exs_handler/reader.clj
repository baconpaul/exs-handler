(ns exs-handler.reader
  (:require [clojure.java.io :refer [file output-stream input-stream]])
  (:import [java.nio ByteBuffer ByteOrder])
  )

;; TODO: Multimethod reader
(defmulti unpack-chunk :sig)



(defmethod unpack-chunk :zone [dat]
  (let [bb (ByteBuffer/allocate (:size dat))
        _  (doto bb
             (.order (ByteOrder/LITTLE_ENDIAN))
             (.put (byte-array (:chunkdata dat)) 0 (:size dat))
             (.flip)
             )
        ;; side effecting
        gb #(.get bb)
        gi #(.getInt bb)

        sb #(do (gb) %)
        si #(do (gi) %)
        
        ]
    (-> dat

        ;; order matters
        (assoc :ospt (gb))
        (assoc :rootnote (gb))
        (assoc :finetune (gb))
        (assoc :pan (gb))
        (assoc :volume-adjust (gb))
        sb
        (assoc :start-note (gb))
        (assoc :end-note (gb))
        sb
        (assoc :min-vel (gb))
        (assoc :max-vel (gb))
        sb
        
        (dissoc :chunkdata)
        )))

(defmethod unpack-chunk :default [dat]
  (-> dat
      (assoc :error "Parser not implemented")
      (dissoc :chunkdata)
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

(defn unpack-common-84 [bts]
  (let [bb (ByteBuffer/allocate 84)
        _  (doto bb
             (.order (ByteOrder/LITTLE_ENDIAN))
             (.put (byte-array bts) 0 84)
             (.flip)
             )
        sig (.getInt bb)
        sz  (.getInt bb)
        id  (.getInt bb)
        unk (->> bts (drop 12) (take 8))
        nm (String. (byte-array (take-while (comp not zero?) (drop 20 bts))) "UTF-8")
        ]
    {:sig
     (condp = sig
       0x00000101 :header
       0x01000101 :zone
       0x02000101 :group
       0x03000101 :sample
       0x04000101 :param
       
       {:unknown-type sig :bytes bts}
       )
     :size sz
     :id id
     :unk unk
     :name nm
     }
    )
  )

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

(->  (read-file "/Users/paul/Music/Audio Music Apps/Sampler Instruments/Linn Drum Machine.exs")
     :chunks
     (#(map (juxt :sig :unk) %)))

(read-file "/Users/paul/Music/Audio Music Apps/Sampler Instruments/Linn Drum Machine.exs")

