(ns exs-handler.writer
  (:require [clojure.java.io :refer [file output-stream input-stream]])
  (:import [java.nio ByteBuffer ByteOrder])
  )

(defn gen-common-84 [type id size name]
  (let [c84 (byte-array 84)
        bb (ByteBuffer/allocate 84)
        ]
    (doto bb
      (.putInt type)
      (.putInt size)
      (.putInt id)
      (.putInt 0) ;; fix this!
      (.put (byte  84))
      (.put (byte  66))
      (.put (byte  79))
      (.put (byte  83)) ;; TBOS
      (.put (.getBytes name))
      (.put (byte-array (.remaining bb)))
      (.flip)
      (.get c84 0 84)
      )
    c84
    )
  )

(defn gen-header [])

(->> (gen-common-84 0x00000101 -1 80 "AXqSample.exs")
     (map identity)
     )
