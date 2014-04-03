(ns common-clojure.core)

(use '[monads core cont util])
(use 'sanity.core)
(use 'sanity.improvements)
(use 'sanity.reader)
(use 'clojure.pprint)
(require '[clatrix.core :as c])
(require '[clojure.string :as s])
(require '[clojure.java.io :as jio])
(require '[taoensso.timbre :as log])
(require '[clansi.core :as color])

;; Load up OpenCV
(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)

(import java.io.StringWriter java.io.File)
(import 'java.text.NumberFormat)

(combine-namespaces
 i
 incanter.bayes incanter.censored incanter.charts incanter.core incanter.datasets
 incanter.distributions incanter.excel incanter.infix incanter.interpolation
 incanter.io incanter.latex incanter.mongodb incanter.optimize incanter.pdf
 incanter.som
 incanter.stats incanter.svg incanter.symbolic incanter.zoo)

;; Submit to bwo/monads
(defmacro munless
 "Execute the computation acc if p is falsy."
 [p acc]
 `(if ~p
   ~(return nil)
   ~acc))

(define (ptrace a)
 (pprint a)
 a)

(define (listy? a) (or (seq? a) (list? a)))

(define (symbol->string s) (str s))
(define (string->symbol s) (symbol s))

(define (member o l) (some #{o} l))

(defn percent
 ([n] (.format (NumberFormat/getPercentInstance) n))
 ([n precision] (let [f (NumberFormat/getPercentInstance)]
                 (.setMaximumFractionDigits f precision)
                 (.format f n))))

(defn x [v] (nth v 0))
(defn y [v] (nth v 1))
(defn z [v] (nth v 2))
(defn w [v] (nth v 3))

(defn r [v] (nth v 0))
(defn g [v] (nth v 1))
(defn b [v] (nth v 2))
(defn a [v] (nth v 3))

(defn hsv-degrees->hsv-radians [c] (vector (degrees->radians (x c)) (y c) (z c)))
(defn hsv-radians->hsv-degrees [c] (vector (radians->degrees (x c)) (y c) (z c)))
(defn pack-color [c] (+ (* 100 100 (x c)) (* 100 (y c)) (z c)))
(defn unpack-color [c] (vector (mod (floor (/ c (* 100 100))) 360)
                               (mod (floor (/ c 100)) 100)
                               (mod c 100)))

(defn transpose-lists [ls]
 (loop [r (map list (first ls)) ls (rest ls)]
  (if (empty? ls)
   (map reverse r)
   (recur (map (fn [x l] (cons x l)) (first ls) r)
          (rest ls)))))

;;; Linear algebra

(defn dot [u v] (reduce + 0 (map * u v)))
(defn v+ [a b] (map-vector + a b))
(defn v- [a b] (map-vector - a b))
(defn k*v [k v] (map-vector (fn [x] (* k x)) v))
(defn k+v [k v] (map-vector (fn [x] (+ k x)) v))
(defn v= [u v] (every? = u v))
(defn rotate-90 [u] (vector (- (y u)) (x u)))
(defn rotate-180 [u] (vector (- (x u)) (- (y u))))
(defn rotate-270 [u] (vector (y u) (- (x u))))
(defn perpendicular? [u v] (zero? (dot u v)))
(defn parallel? [u v] (perpendicular? (rotate-90 u) v))
(defn magnitude-squared [v] (dot v v))
(defn magnitude [v] (sqrt (magnitude-squared v)))
(defn unit [v] (k*v (/ (magnitude v)) v))
(defn distance-squared [u v] (magnitude-squared (v- v u)))
(defn distance [u v] (sqrt (distance-squared u v)))

(defn matrix-ref [m i j]
 (if (c/matrix? m)
  (c/get ^clatrix.core.Matrix m i j)
  (vector-ref (vector-ref m i) j)))

(defn image-ref [i x y] (matrix-ref i y x))

(defn matrix-rows [m] (c/nrows m))
(defn matrix-columns [m] (c/ncols m))

(define (but-last l) (reverse (rest (reverse l))))

(define (fields l) (s/split l #"\s+"))

(define (file-exists? file) "Check if the file exists"
 (.exists (jio/as-file file)))

(define (words l) (s/split l #"\s+"))
(define (unwords l) (apply str l))

(define (lines l) (s/split l #"\n+"))
(define (unlines l) (s/join "\n" l))

;;; Zip

(import '(java.util.zip ZipFile ZipEntry))
(import '(java.nio.file Files Paths Path))
(import '(java.nio.charset Charset))
(import '[org.apache.commons.io IOUtils])
(import '[java.io InputStream])

(define (with-zip-file f ^String fileName) (with-open [zip (ZipFile. fileName)] (f zip)))

(define (zip:read-file ^ZipFile zip filename)
 (s/split (IOUtils/toString ^InputStream (.getInputStream zip (.getEntry zip filename))) #"\n"))

(define (zip:read-file-to-buffer ^ZipFile  zip filename)
 (IOUtils/toByteArray ^InputStream (.getInputStream zip (.getEntry zip filename))))

;;; OpenCV

(import '[org.opencv.core Mat Size CvType MatOfByte Core Point Scalar]
        '[org.opencv.highgui Highgui VideoCapture]
        '[org.opencv.imgproc Imgproc])

;; Raw video API

(define (open-video% ^String filename)
 (let [v (VideoCapture.)]
  (.open v filename)
  (when (not (.isOpened v)) (throw (ex-info "Can't open" {:filename filename})))
  (.grab v)
  v))

(define (close-video% ^VideoCapture video) (.release video))

(define (with-video% filename f)
 (let [v (open-video% filename)
       r (f v)]
  (close-video% v)
  r))

(define (next-frame% ^VideoCapture video) (.grab video))

(define (compute-video-length% video-path)
 (with-video%
  video-path
  (fn [video]
   (loop [i 0] (if (next-frame% video) (recur (+ i 1)) i)))))

(define (video-length video-path)
 (if (:length video-path)
  video-path
  (let [length-file (replace-extension video-path "video-length")]
   (or (if (file-exists? length-file)
        (let [data (read-object-from-file length-file)
              mod-time (file-change-time video-path)]
         (if (and (seq? data) (= (length data) 2) (every? number? data)
                  (= (second data) mod-time))
          (first data)
          false))
        false)
       ;; * matters because we want the time computed before the
       ;; video length to avoid race conditions
       (let ((mod-time (file-change-time video-path))
             (l (compute-video-length% video-path)))
        (write-object-to-file (list l mod-time) length-file)
        l)))))

;; High-level video API

(defrecord opencv-video [^VideoCapture handle ^String file ^int length width height])

(define (open-video ^String filename)
 (let [video (open-video% filename)
       ^Mat m (Mat.)]
  (.retrieve ^VideoCapture video m)
  (->opencv-video video filename (video-length filename)
                  (.width m)
                  (.height m))))

(define (close-video ^opencv-video video) (close-video% (:handle video)))

(define (with-video filename f)
 (let [v (open-video filename)
       r (f v)]
  ;; FIXME This is a hack because the JVM does not gc and run
  ;; finalizers often enough to clean up OpenCV's garbage in a timely
  ;; fashion. Mat objects are tiny on the JVM's heap but large on the
  ;; system heap. This means that only a few kb JVM heap space can
  ;; correspond to enogh memory to exhaust all RAM.
  (System/gc)
  (close-video v)
  r))

(define (next-frame! ^opencv-video video) (next-frame% (:handle video)))
(define (read-frame ^opencv-video video) (let [m (Mat.)] (if (.read ^VideoCapture (:handle video) m) m nil)))
(define (peek-frame ^opencv-video video) (let [m (Mat.)] (if (.retrieve ^VideoCapture (:handle video) m) m nil)))

;;; OpenCV Image

(defn draw-rectangle [image box colour thickness]
 (Core/rectangle image
                 (Point. (nth box 0) (nth box 1))
                 (Point. (nth box 2) (nth box 3))
                 (Scalar. (nth colour 2) (nth colour 1) (nth colour 0))
                 thickness))

(defn image-load [s] (Highgui/imread s))
(defn image-save [s i] (Highgui/imwrite s i))

(defrecord image-buffer [^"[B" buffer ^long width ^long height ^long channels ^clojure.lang.Symbol format])
(. clojure.pprint/simple-dispatch addMethod image-buffer (fn [& more] (print more)))

(defn image->image-buffer [^org.opencv.core.Mat i]
 (let ((buffer (byte-array (* (.width i) (.height i) (.channels i)))))
  (.get i 0 0 buffer)
  (->image-buffer buffer (.width i) (.height i) (.channels i) 'bgr)))

;; Show

(import '(javax.swing JFrame JLabel JTextField JButton ImageIcon)
        '(javax.imageio ImageIO)
        '(java.io ByteArrayInputStream)
        '(java.awt.event ActionListener)
        '(java.awt GridLayout))

(define (imshow ^Mat img)
 (let [m (MatOfByte.)]
  (Highgui/imencode ".jpg" img m)
  (let [frame (JFrame.)]
   (.add
    (.getContentPane frame)
    (JLabel. (ImageIcon. (ImageIO/read (ByteArrayInputStream. (.toArray m))))))
   (.pack frame)
   (.setVisible frame true))))

;; Incanter integration
(defmethod i/view org.opencv.core.Mat ([obj & options] (imshow obj)))
(define view i/view)

(define (video-first-frame video-path) 1)
(define (video-last-frame video-path) (video-length video-path))

(define (for-each-frame f v)
 (for-each-m-n f (video-first-frame v) (video-last-frame v)))

(define (for-each-frame-reversed f v)
 (for-each-m-n-dec f (video-last-frame v) (video-first-frame v)))

(define (map-frame f v)
 (map-m-n f (video-first-frame v) (video-last-frame v)))

(define (map-frame-indexed f v)
 (let ((first-frame (video-first-frame v)))
  (map-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (video-last-frame v))))

(define (for-each-frame-indexed f v)
 (let ((first-frame (video-first-frame v)))
  (for-each-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (video-last-frame v))))

(define (map-frame-indexed-but-last f v)
 (let ((first-frame (video-first-frame v)))
  (map-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (- (video-last-frame v) 1))))

(define (for-each-frame-indexed-but-last f v)
 (let ((first-frame (video-first-frame v)))
  (for-each-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (- (video-last-frame v) 1))))

(define (for-each-frame-but-last f v)
 (for-each-m-n f (video-first-frame v) (- (video-last-frame v) 1)))

(define (map-frame-but-last f v)
 (map-m-n f (video-first-frame v) (- (video-last-frame v) 1)))

(define (map-frame-pair individual-f pair-f video-path)
 (let ((first-frame (video-first-frame video-path))
       (last-frame (video-last-frame video-path)))
  (loop ((n (+ first-frame 1))
         (prev (individual-f first-frame))
         (result '()))
   (if (> n last-frame)
    (reverse result)
    (let ((next (individual-f n)))
     (recur (+ n 1) next (cons (pair-f prev next) result)))))))

(define (for-each-frame-pair individual-f pair-f video-path)
 (let ((first-frame (video-first-frame video-path))
       (last-frame (video-last-frame video-path)))
  (loop ((n (+ first-frame 1))
         (prev (individual-f first-frame)))
   (unless (> n last-frame)
    (let ((next (individual-f n)))
     (pair-f prev next)
     (recur (+ n 1) next))))))

(define (map-image-from-video-indexed f video-path)
 (with-video
  video-path
  (lambda (video)
   (map-frame-indexed
    ;; Frame-nr is the current frame number -- the first frame may not be zero
    ;; Index is the frame offset (starting at 0) in the video file
    (lambda (frame-nr index)
     (let ((result (f frame-nr index (peek-frame video))))
      (next-frame! video)
      result))
    video-path))))

(define (for-each-image-from-video-indexed f video-path)
 (with-video
  video-path
  (lambda (video)
   (for-each-frame-indexed
    ;; Frame-nr is the current frame number -- the first frame may not be zero
    ;; Index is the frame offset (starting at 0) in the video file
    (lambda (frame-nr index)
     (f frame-nr index (peek-frame video))
     (next-frame! video))
    video-path))))

(define (for-each-image-from-video-indexed-but-last f video-path)
 (with-video
  video-path
  (lambda (video)
   (for-each-frame-indexed-but-last
    ;; Frame-nr is the current frame number -- the first frame may not be zero
    ;; Index is the frame offset (starting at 0) in the video file
    (lambda (frame-nr index)
     (f frame-nr index (peek-frame video))
     (next-frame! video))
    video-path))))

(define (map-image-pair-from-video individual-f pair-f video-path)
 ;; individual-f :: frame-nr -> imlib -> a
 ;; pair-f :: a -> a -> b
 (with-video
  video-path
  (lambda (video)
   (map-frame-pair
    (lambda (frame-nr)
     (let ((frame-data (peek-frame video)))
      (next-frame! video)
      (individual-f frame-nr frame-data)))
    pair-f
    video-path))))

(define (for-each-image-pair-from-video-indexed individual-f pair-f video-path)
 ;; individual-f :: frame-nr -> index -> imlib -> a
 ;; pair-f :: a -> a -> b
 (letfn [(for-each-frame-pair-indexed [individual-f pair-f video-path]
           (let ((first-frame (video-first-frame video-path))
                 (last-frame (video-last-frame video-path)))
            (loop ((n (+ first-frame 1))
                   (i 1)
                   (prev (individual-f first-frame 0)))
             (unless (> n last-frame)
              (let ((next (individual-f n i)))
               (pair-f prev next)
               (recur (+ n 1) (+ i 1) next))))))]
  (with-video
   video-path
   (lambda (video)
    (for-each-frame-pair-indexed
     (lambda (frame-nr index)
      (let ((frame-data (peek-frame video)))
       (next-frame! video)
       (individual-f frame-nr index frame-data)))
     pair-f
     video-path)))))

(define (for-each-image-pair-from-video individual-f pair-f video)
 (for-each-image-pair-from-video-indexed
  (lambda (frame-nr index frame-data)
   (individual-f frame-nr frame-data))
  pair-f
  video))

;;; Optical flow

(import '(java.nio ByteOrder HeapByteBuffer))
(import '(java.nio.file Files Paths Path))
(import 'org.jblas.DoubleMatrix)
(use '[gloss core io])

(defrecord optical-flow [x y])

(defcodec flo
 (header
  (ordered-map
   :tag (string :utf-8 :length 4)
   :width :uint32-le
   :height :uint32-le)
  (fn [h]
   (compile-frame
    (ordered-map
     :header h
     :values (finite-block (* 8 (:width h) (:height h))))))
  (fn [h] (compile-frame (finite-block (* (:width h) (:height h)))))))

(defn read-flo-from-buffer [^"[B" buffer]
 (let [d (decode flo buffer)
       w (:width (:header d))
       h (:height (:header d))
       ^"[[D" xs (make-array Double/TYPE h w)
       ^"[[D" ys (make-array Double/TYPE h w)
       buffer (.order ^HeapByteBuffer (first (:values d)) ByteOrder/LITTLE_ENDIAN)]
  (dotimes [i h]
   (dotimes [j w]
    (aset-double (aget xs i) j (.getFloat ^HeapByteBuffer buffer))
    (aset-double (aget ys i) j (.getFloat ^HeapByteBuffer buffer))))
  (->optical-flow (c/matrix (DoubleMatrix. xs) nil) (c/matrix (DoubleMatrix. ys) nil))))

(defn read-binary-file [filename]
 (Files/readAllBytes (Paths/get filename (into-array [""]))))

;; TODO Boxed!
(defn integral-matrix-area [m x1 y1 x2 y2]
 (let ((width (matrix-columns m))
       (height (matrix-rows m))
       (x1 (round x1)) (y1 (round y1))
       (x2 (round x2)) (y2 (round y2))
       (x1' (if (>= (+ x1 1) width) (- width 2) x1))
       (y1' (if (>= (+ y1 1) height) (- height 2) y1))
       (x2' (if (>= (+ x2 1) width) (- width 2) x2))
       (y2' (if (>= (+ y2 1) height) (- height 2) y2)))
  (+ (image-ref m x1 y1)
     (- (image-ref m (+ x2 1) y1))
     (- (image-ref m x1 (+ y2 1)))
     (image-ref m (+ x2 1) (+ y2 1)))))

;; Note that this internally clips to the border of the image
(defn average-integral-optical-flow [optical-flow x1 y1 x2 y2]
 (k*v
  (/ 2 (* (+ (- x2 x1) 1) (+ (- y2 y1) 1)))
  [(integral-matrix-area (:x optical-flow) x1 y1 x2 y2)
   (integral-matrix-area (:y optical-flow) x1 y1 x2 y2)]))

(defn average-flow-in-box [box flow-transformation]
 ;; FIXME Expose this parameter
 ;; This 2 is because of scale!
 (k*v 2
      (average-integral-optical-flow
       flow-transformation
       (/ (nth box 0) 2)
       (/ (nth box 1) 2)
       (/ (nth box 2) 2)
       (/ (nth box 3) 2))))

(defn clip [v low high] (max (min v high) low))

(defn sigmoid [t a b] (/ (+ 1 (Math/exp (- (* b (- t a)))))))

(defn number->padded-string-of-length [n padding]
 (format (format "%%0%sd" padding) n))

(defn for-each-pair [f as bs]
 (for-each (fn [a] (for-each (fn [b] (f a b)) bs)) as))

(defn panic [s & more] (throw (ex-info s {:args more})))
(defn fuck-up [] (throw (ex-info "fuck-up" {})))

(defn read-file [filename]
 (with-open [r (java.io.PushbackReader. (clojure.java.io/reader filename))]
  (binding [*read-eval* false]
   (read r))))

;; Map indexed flips the order of arguments.. this is _insane_
;; My local one in the current namespace is corect
(defn map-indexed [f l]
 ;; needs work: To eliminate REVERSE.
 (loop [i 0 l l c '()]
  (if (null? l)
   (reverse c)
   (recur (+ i 1) (rest l) (cons (f (first l) i) c)))))

;; TODO I want this to figure out the name of the current function if possible
(log/set-config!
 [:fmt-output-fn]
 (fn [{:keys [level throwable message timestamp hostname ns]}
      ;; Any extra appender-specific opts:
      & [{:keys [nofonts?] :as appender-fmt-output-opts}]]
  (format "%s %s -- %s%s"
          (color/style (-> level name s/upper-case)
                       (case level
                        :trace  :blue
                        :debug  :cyan
                        :info   :green
                        :warn   :magenta
                        :error  :red
                        :fatal  :bg-red
                        :report :yellow))
          (s/replace
           (third (drop-while #(not (= "taoensso.timbre$send_to_appenders_BANG_" %))
                              (map #(.getClassName %) (.getStackTrace (Throwable.)))))
           #"^([^$]+)\$" "$1/")
          (or message "")
          (or (log/stacktrace throwable "\n" (when nofonts? {})) ""))))

;; default level
(log/set-level! :info)

(define (minimuml l) (minimum (map minimum l)))
(define (maximuml l) (maximum (map maximum l)))

;; These suck. They allow things like #t1
;; But for now they're going to have to do
(dispatch-reader-macro \t (fn [_ _] true))
(dispatch-reader-macro \f (fn [_ _] false))

;; TODO Need to replace the default
;; (ns-unmap *ns* 'map-n)
;; (defn map-n
;;  ([f n] (map f (range 0 n)))
;;  ([f m n] (loop ((i m) (c [])) (if (> i n) c (recur (+ i 1) (conj c (f i))))))
;;  ([f m n stride] (loop ((i m) (c [])) (if (> i n) c (recur (+ i stride) (conj c (f i)))))))
