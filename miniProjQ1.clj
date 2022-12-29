;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code --- importing necessary things.
;; **

;; @@
(use 'nstools.ns)
(ns+ template
  (:like anglican-user.worksheet))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[nil,nil]"}
;; <=

;; @@
(def N 10000)
(def x_0 0)
(def sigma 5)
(def burn-in 100)
(def lag 20)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/N</span>","value":"#'template/N"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/x_0</span>","value":"#'template/x_0"}],"value":"[#'template/N,#'template/x_0]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/sigma</span>","value":"#'template/sigma"}],"value":"[[#'template/N,#'template/x_0],#'template/sigma]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/burn-in</span>","value":"#'template/burn-in"}],"value":"[[[#'template/N,#'template/x_0],#'template/sigma],#'template/burn-in]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/lag</span>","value":"#'template/lag"}],"value":"[[[[#'template/N,#'template/x_0],#'template/sigma],#'template/burn-in],#'template/lag]"}
;; <=

;; @@
(defn pi [x]
  (let [pow (Math/pow (- x (/ 1 2)) 4)
        expo (Math/exp (* -3 (Math/abs x)))
        cosin (Math/pow (Math/cos x) 2)]
    (* (* pow expo) cosin)
    ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/pi</span>","value":"#'template/pi"}
;; <=

;; @@
(defn quantise [i]
  (Math/floor (+ i 0.5)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/quantise</span>","value":"#'template/quantise"}
;; <=

;; **
;;; Proposal:
;; **

;; @@
(defn q [x_t]
  (let [epsilon (sample* (normal 0 sigma))]
    (quantise (+ x_t epsilon))
  )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/q</span>","value":"#'template/q"}
;; <=

;; **
;;; Functions to treat the obtained sequence to thin out using the lag parameter and cut out the burn-in period:
;; **

;; @@
(defn rev [l]
  (reduce conj (list) l))

(defn clean-out [l]
  (let [burnt (drop (+ burn-in 1) l)
        thinned (take-nth lag burnt)
         ]
    thinned))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/rev</span>","value":"#'template/rev"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/clean-out</span>","value":"#'template/clean-out"}],"value":"[#'template/rev,#'template/clean-out]"}
;; <=

;; @@
(defn MHsampler [N x_0 lag burn-in]
  (loop [i 0 s (list x_0)]
    (if (= i (+ burn-in (* N lag)))
      (clean-out (rev s))
      (recur (inc i)
             (conj s (let [x_last (first s)
                           y (q x_last)
                           acceptance (/ (pi y) (pi x_last))
                           u (sample* (uniform-continuous 0 1))
                           ]
                       (if (<= u acceptance)
                         y
                         x_last
                       )))
      )
  )
  )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/MHsampler</span>","value":"#'template/MHsampler"}
;; <=

;; @@
(def sampled (MHsampler N x_0 lag burn-in))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/sampled</span>","value":"#'template/sampled"}
;; <=

;; @@
(plot/histogram sampled :bins 30)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"b0c3b7cf-eca2-483d-943c-d652b4d359bb","values":[{"x":-6.0,"y":0},{"x":-5.6,"y":2.0},{"x":-5.199999999999999,"y":0.0},{"x":-4.799999999999999,"y":3.0},{"x":-4.399999999999999,"y":0.0},{"x":-3.9999999999999987,"y":63.0},{"x":-3.5999999999999988,"y":0.0},{"x":-3.199999999999999,"y":0.0},{"x":-2.799999999999999,"y":999.0},{"x":-2.399999999999999,"y":0.0},{"x":-1.9999999999999991,"y":928.0},{"x":-1.5999999999999992,"y":0.0},{"x":-1.1999999999999993,"y":0.0},{"x":-0.7999999999999993,"y":4057.0},{"x":-0.39999999999999925,"y":0.0},{"x":7.771561172376096E-16,"y":3461.0},{"x":0.4000000000000008,"y":0.0},{"x":0.8000000000000008,"y":0.0},{"x":1.2000000000000008,"y":39.0},{"x":1.600000000000001,"y":0.0},{"x":2.000000000000001,"y":129.0},{"x":2.400000000000001,"y":0.0},{"x":2.8000000000000007,"y":0.0},{"x":3.2000000000000006,"y":294.0},{"x":3.6000000000000005,"y":0.0},{"x":4.000000000000001,"y":24.0},{"x":4.400000000000001,"y":0.0},{"x":4.800000000000002,"y":0.0},{"x":5.200000000000002,"y":0.0},{"x":5.600000000000002,"y":0.0},{"x":6.000000000000003,"y":1.0},{"x":6.400000000000003,"y":0}]}],"marks":[{"type":"line","from":{"data":"b0c3b7cf-eca2-483d-943c-d652b4d359bb"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"b0c3b7cf-eca2-483d-943c-d652b4d359bb","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"b0c3b7cf-eca2-483d-943c-d652b4d359bb","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"b0c3b7cf-eca2-483d-943c-d652b4d359bb\", :values ({:x -6.0, :y 0} {:x -5.6, :y 2.0} {:x -5.199999999999999, :y 0.0} {:x -4.799999999999999, :y 3.0} {:x -4.399999999999999, :y 0.0} {:x -3.9999999999999987, :y 63.0} {:x -3.5999999999999988, :y 0.0} {:x -3.199999999999999, :y 0.0} {:x -2.799999999999999, :y 999.0} {:x -2.399999999999999, :y 0.0} {:x -1.9999999999999991, :y 928.0} {:x -1.5999999999999992, :y 0.0} {:x -1.1999999999999993, :y 0.0} {:x -0.7999999999999993, :y 4057.0} {:x -0.39999999999999925, :y 0.0} {:x 7.771561172376096E-16, :y 3461.0} {:x 0.4000000000000008, :y 0.0} {:x 0.8000000000000008, :y 0.0} {:x 1.2000000000000008, :y 39.0} {:x 1.600000000000001, :y 0.0} {:x 2.000000000000001, :y 129.0} {:x 2.400000000000001, :y 0.0} {:x 2.8000000000000007, :y 0.0} {:x 3.2000000000000006, :y 294.0} {:x 3.6000000000000005, :y 0.0} {:x 4.000000000000001, :y 24.0} {:x 4.400000000000001, :y 0.0} {:x 4.800000000000002, :y 0.0} {:x 5.200000000000002, :y 0.0} {:x 5.600000000000002, :y 0.0} {:x 6.000000000000003, :y 1.0} {:x 6.400000000000003, :y 0})}], :marks [{:type \"line\", :from {:data \"b0c3b7cf-eca2-483d-943c-d652b4d359bb\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"b0c3b7cf-eca2-483d-943c-d652b4d359bb\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"b0c3b7cf-eca2-483d-943c-d652b4d359bb\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=
