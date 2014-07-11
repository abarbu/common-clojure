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

(import java.io.StringWriter java.io.File)
(import 'java.text.NumberFormat)

;; The following 3 incanter packages aren't included below because
;; even just including them requires an X connection. This seems like
;; an upstream bug.
;;
;; incanter.svg incanter.pdf incanter.charts 

(combine-namespaces
 i
 incanter.bayes incanter.censored incanter.core incanter.datasets
 incanter.distributions incanter.excel incanter.infix incanter.interpolation
 incanter.io incanter.latex incanter.mongodb incanter.optimize
 incanter.som incanter.stats incanter.symbolic incanter.zoo)

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

(define (resource-path x)
 (.getPath (clojure.java.io/resource x)))

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
(defn map-indexed' [f l]
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
