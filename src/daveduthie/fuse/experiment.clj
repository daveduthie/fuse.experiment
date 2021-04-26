(ns daveduthie.fuse.experiment
  (:import
   (java.io File)
   (java.nio ByteBuffer)
   (java.nio.file Paths)
   (jnr.ffi Pointer)
   (ru.serce.jnrfuse ErrorCodes FuseStubFS FuseFillDir)
   (ru.serce.jnrfuse.struct FileStat FuseFileInfo)))

(defn enoent-error []
  (* -1 (ErrorCodes/ENOENT)))

(def HELLO_STR "Hello World!")
(def HELLO_PATH "/hello")

(def LOGGER (agent nil))
(defn log [msg] (send-off LOGGER (fn [_] (println (pr-str msg)) nil)))

(defn fuse-custom-mount
  []
  (proxy [FuseStubFS] []
    (getattr [^String path ^FileStat stat]
      (log {:in :getattr :path path})
      (cond
        (= path "/")      (do (-> stat .-st_mode (.set (bit-or FileStat/S_IFDIR 0755)))
                              (-> stat .-st_nlink (.set 2))
                              0)
        (= path "/hello") (do (-> stat .-st_mode (.set (bit-or FileStat/S_IFREG 0444)))
                              (-> stat .-st_nlink (.set 1))
                              (-> stat .-st_size (.set (count (.getBytes HELLO_STR))))
                              0)
        :else             (enoent-error)))
    (readdir [^String path
              ^Pointer buf
              ^FuseFillDir filt
              ^Long offset
              ^FuseFileInfo fi]
      ;; Here we choose what to list.
      (log {:in :readdir})
      (if (= "/" path)
        (do (.apply filt buf "." nil 0)
            (.apply filt buf ".." nil 0)
            (.apply filt buf (.substring HELLO_PATH 1) nil 0)
            0)
        (enoent-error)))
    (open [^String path ^FuseFileInfo fi]
      ;; Here we handle errors on opening
      (log {:in :open :path path})
      (if (= "/hello" path) 0 (enoent-error)))
    (read [^String path ^Pointer buf ^Long size ^Long offset ^FuseFileInfo fi]
      ;; Here we read the contents
      (log {:in :read :path path})
      (if-not (= "/hello" path)
        (enoent-error)
        (let [bytes      (.getBytes HELLO_STR)
              len        (count bytes)
              to-read    (min (- len offset))
              contents   (ByteBuffer/wrap bytes)
              bytes-read (byte-array to-read)]
          (doto contents (.position offset) (.get bytes-read 0 to-read))
          (-> buf
              (.put 0 bytes-read 0 to-read))
          (.position contents 0)
          to-read)))))

(def stub-atom (atom nil))

(defn string-to-uri [s]
  (-> s File. .toURI))

(defn uri-to-path [s]
  (Paths/get s))

(defn string-to-path [s]
  (-> s string-to-uri uri-to-path))

(defn mount-it!
  [dir]
  (let [stub (fuse-custom-mount)]
    (log {:stub stub})
    (future (reset! stub-atom stub)
            ;; params: path blocking debug options
            (->
              stub
              (.mount
               (string-to-path dir)
               true
               true
               (into-array String []))))))

(comment
  (string-to-path "/tmp/fuse-experiments3"))

(defn umount-it! []
  (-> @stub-atom .umount))

(defn cleanup-hooks [mnt]
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread. (fn []
              (println "Unmounting " mnt)
              (umount-it!)))))

(defn main
  "I don't do a whole lot ... yet."
  [{:keys [dir]}]
  (cleanup-hooks dir)
  (println "Mounting " dir)
  (println (format "Try going to %s and running ls." dir))
  (deref (mount-it! dir)))

(comment
  (def DIR "/tmp/fuse-experiment2")
  (cleanup-hooks DIR)
  (deref (mount-it! DIR))
  @stub-atom
  (umount-it!)
  ,)
