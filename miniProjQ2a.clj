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
;utility function
;list reversal function
(defn rev [l]
  (reduce conj (list) l))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/rev</span>","value":"#'template/rev"}
;; <=

;; **
;;; Q2 a) ii)
;; **

;; **
;;; The code segment below creates the template for an Anglican query, modelling the latent variables without modelling the observable ones (no y's).
;;; It iteratively creates the variables x-i from the previous x-(i-1).
;;; We will reuse this same format in an actual Anglican query in Q2 a) iii), which will include the observations made.
;; **

;; @@
(defn q2aTwo [S T P]
  (let [x_1 (sample* (uniform-discrete 1 (inc S)))
        latents (loop [cnt 2 ls (list x_1)]
                  (if (> cnt T)
                    (rev ls)
                    (recur (inc cnt) (conj ls (inc (sample* (discrete (nth P (first ls))))))))
                  )
        ]
    latents
    ))

(q2aTwo S T P)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/q2aTwo</span>","value":"#'template/q2aTwo"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"(4 2 4 4 4 1)"}],"value":"[#'template/q2aTwo,(4 2 4 4 4 1)]"}
;; <=

;; **
;;; Q2 a) iii)
;; **

;; @@
;data from problem statement
(def S 4)
(def T 6)
(def P '(() (0.1 0.4 0.3 0.2) (0.1 0.1 0.6 0.2) (0.1 0.2 0.5 0.2) (0.2 0.3 0.4 0.1)))
(def obs '(0.5 1.5 0.9 0.0 2.5 3.0))
P
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/S</span>","value":"#'template/S"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/T</span>","value":"#'template/T"}],"value":"[#'template/S,#'template/T]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/P</span>","value":"#'template/P"}],"value":"[[#'template/S,#'template/T],#'template/P]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/obs</span>","value":"#'template/obs"}],"value":"[[[#'template/S,#'template/T],#'template/P],#'template/obs]"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"},{"type":"html","content":"<span class='clj-double'>0.4</span>","value":"0.4"},{"type":"html","content":"<span class='clj-double'>0.3</span>","value":"0.3"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"(0.1 0.4 0.3 0.2)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"},{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"},{"type":"html","content":"<span class='clj-double'>0.6</span>","value":"0.6"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"(0.1 0.1 0.6 0.2)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"},{"type":"html","content":"<span class='clj-double'>0.5</span>","value":"0.5"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"(0.1 0.2 0.5 0.2)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"},{"type":"html","content":"<span class='clj-double'>0.3</span>","value":"0.3"},{"type":"html","content":"<span class='clj-double'>0.4</span>","value":"0.4"},{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"}],"value":"(0.2 0.3 0.4 0.1)"}],"value":"(() (0.1 0.4 0.3 0.2) (0.1 0.1 0.6 0.2) (0.1 0.2 0.5 0.2) (0.2 0.3 0.4 0.1))"}],"value":"[[[[#'template/S,#'template/T],#'template/P],#'template/obs],(() (0.1 0.4 0.3 0.2) (0.1 0.1 0.6 0.2) (0.1 0.2 0.5 0.2) (0.2 0.3 0.4 0.1))]"}
;; <=

;; **
;;; 
;; **

;; **
;;; This query includes observations, the likelihood function, and the transition matrix P. Because of technicalities the resulting output's order for the latent variables is reversed. We will fix that in the utility function result-processor.
;; **

;; @@
(defquery q2aThree [S T P obs]
  (let [x_1 (sample (uniform-discrete 1 (inc S)))
        latents (loop [cnt 2 ls (list x_1)]
                  (if (> cnt T)
                    ls
                    (recur (inc cnt) (conj ls (inc (sample (discrete (nth P (first ls))))))))
                  )
        ]
    (map 
      (fn [x_i y_i] (observe (normal x_i 1) y_i))
      latents
      obs)
    latents
    ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/q2aThree</span>","value":"#'template/q2aThree"}
;; <=

;; **
;;; Result element is reversed!!!!!
;; **

;; @@
(def N 1750)
(def head (take N (doquery :importance q2aThree [S T P (rev obs)])))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/N</span>","value":"#'template/N"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/head</span>","value":"#'template/head"}],"value":"[#'template/N,#'template/head]"}
;; <=

;; **
;;; Q2 a) iv)
;; **

;; **
;;; Some utility functions. The function result-process reverses the result array of latent variables, and formats it to one-hot encoding.
;; **

;; @@
(defn one-hotter [i S]
  (let [zero-tail (apply list (replicate (- S (dec i)) 0))
        popped (pop zero-tail)
        one-added (conj popped 1)
        front (replicate (dec i) 0)]
    (apply list (concat front one-added))))

(defn result-processor [ls]
  (let [reversed (rev ls)
        repeat-ss (replicate T S)
        onehot (map one-hotter reversed repeat-ss)
         ]
    onehot))

(defn expoweight [elmt]
  (exp (:log-weight elmt)))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/one-hotter</span>","value":"#'template/one-hotter"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/result-processor</span>","value":"#'template/result-processor"}],"value":"[#'template/one-hotter,#'template/result-processor]"}
;; <=

;; **
;;; A couple more utility functions to deal with technicalities of manipulating lists of lists in Clojure.
;; **

;; @@
;product of weights and one hot encodings
;multiply
(defn multiplyList [k ls]
  (map (partial * k) ls))

(defn multiplyEncodings [es k]
  (map (partial multiplyList k) es))


;add all of the N matrices together
(defn addList [a b]
  (map + a b))

(defn addEncoding [as bs]
  (map addList as bs))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/multiplyList</span>","value":"#'template/multiplyList"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/multiplyEncodings</span>","value":"#'template/multiplyEncodings"}],"value":"[#'template/multiplyList,#'template/multiplyEncodings]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/addList</span>","value":"#'template/addList"}],"value":"[[#'template/multiplyList,#'template/multiplyEncodings],#'template/addList]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/addEncoding</span>","value":"#'template/addEncoding"}],"value":"[[[#'template/multiplyList,#'template/multiplyEncodings],#'template/addList],#'template/addEncoding]"}
;; <=

;; **
;;; Functions to compute the mean and standard deviation (e.g. here on 10,000 examples). The function k-runs does the above query (part iii) multiple times and computes statistics. The other functions are utility.
;; **

;; @@
(defn one-run [n]
  (let [head (take n (doquery :importance q2aThree [S T P (rev obs)]))
        weights (map expoweight head)
        cleaned (map result-processor (map :result head))
        prodEncoding (map multiplyEncodings cleaned weights)
        prodSum (reduce addEncoding prodEncoding)
        sum-weights (reduce + 0.0 (map expoweight head))]
    [prodSum sum-weights cleaned weights]))


;;ADD ALL RUNS AND DIVIDE BY SUM OF ALL WEIGHTS
(defn aggregate-means [runs normaliser]
  (let [weighted-sum (loop [acc ((first runs) 0) runsLeft (rest runs)]
                       (if (empty? runsLeft)
                         acc
                         (recur (addEncoding acc ((first runsLeft) 0)) (rest runsLeft))))]
    (multiplyEncodings weighted-sum normaliser)))


(defn square [pow base]
  (Math/pow base pow))

(defn sq-list [pow ls]
  (map (partial square pow) ls))
        
(defn sq-dist-w [element negMean w]
  (let [diff (addEncoding element negMean)
        squared (map (partial sq-list 2) diff)]
    (multiplyEncodings squared w)))
        
(defn sum-sq-dist-per-run [run global-mean]
  (let [encodings (run 2)
        ws (run 3)
        neg-mean (multiplyEncodings global-mean -1.0)
        var-sum (loop [acc (sq-dist-w (first encodings) neg-mean (first ws)) encLeft (rest encodings) wLeft (rest ws)]
                  (if (empty? encLeft)
                    acc
                    (recur (addEncoding acc (sq-dist-w (first encLeft) neg-mean (first wLeft))) (rest encLeft) (rest wLeft))))
         ]
    var-sum))

(defn std-dev [runs mean normaliser]
  (let [global-var-sum (loop [acc (sum-sq-dist-per-run (first runs) mean) runsLeft (rest runs)]
                         (if (empty? runsLeft)
                           acc
                           (recur (sum-sq-dist-per-run (first runsLeft) mean) (rest runsLeft))))
        variance (multiplyEncodings global-var-sum normaliser)
         ]
    (map (partial sq-list 0.5) variance))) ;element-wise square root of the variance

;run k times an n-run, return mean and standard deviation
(defn k-runs [k n]
  (let [runs (take k (repeatedly #(one-run n)))
        sum-all-weights (loop [acc ((first runs) 1) runsLeft (rest runs)]
                       (if (empty? runsLeft)
                         acc
                         (recur (+ acc ((first runsLeft) 1)) (rest runsLeft))))
        normaliser (/ 1 sum-all-weights)
        mean (aggregate-means runs normaliser)
        standard-dev (std-dev runs mean normaliser)]
    [mean standard-dev]
    ))

(def meanAndDeviation (k-runs 10 1000))
(meanAndDeviation 0)
(meanAndDeviation 1)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/one-run</span>","value":"#'template/one-run"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/aggregate-means</span>","value":"#'template/aggregate-means"}],"value":"[#'template/one-run,#'template/aggregate-means]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/square</span>","value":"#'template/square"}],"value":"[[#'template/one-run,#'template/aggregate-means],#'template/square]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/sq-list</span>","value":"#'template/sq-list"}],"value":"[[[#'template/one-run,#'template/aggregate-means],#'template/square],#'template/sq-list]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/sq-dist-w</span>","value":"#'template/sq-dist-w"}],"value":"[[[[#'template/one-run,#'template/aggregate-means],#'template/square],#'template/sq-list],#'template/sq-dist-w]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/sum-sq-dist-per-run</span>","value":"#'template/sum-sq-dist-per-run"}],"value":"[[[[[#'template/one-run,#'template/aggregate-means],#'template/square],#'template/sq-list],#'template/sq-dist-w],#'template/sum-sq-dist-per-run]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/std-dev</span>","value":"#'template/std-dev"}],"value":"[[[[[[#'template/one-run,#'template/aggregate-means],#'template/square],#'template/sq-list],#'template/sq-dist-w],#'template/sum-sq-dist-per-run],#'template/std-dev]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/k-runs</span>","value":"#'template/k-runs"}],"value":"[[[[[[[#'template/one-run,#'template/aggregate-means],#'template/square],#'template/sq-list],#'template/sq-dist-w],#'template/sum-sq-dist-per-run],#'template/std-dev],#'template/k-runs]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/meanAndDeviation</span>","value":"#'template/meanAndDeviation"}],"value":"[[[[[[[[#'template/one-run,#'template/aggregate-means],#'template/square],#'template/sq-list],#'template/sq-dist-w],#'template/sum-sq-dist-per-run],#'template/std-dev],#'template/k-runs],#'template/meanAndDeviation]"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.7357906745871806</span>","value":"0.7357906745871806"},{"type":"html","content":"<span class='clj-double'>0.23121408137183</span>","value":"0.23121408137183"},{"type":"html","content":"<span class='clj-double'>0.030878530568161942</span>","value":"0.030878530568161942"},{"type":"html","content":"<span class='clj-double'>0.002116713472826875</span>","value":"0.002116713472826875"}],"value":"(0.7357906745871806 0.23121408137183 0.030878530568161942 0.002116713472826875)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.23776831878467936</span>","value":"0.23776831878467936"},{"type":"html","content":"<span class='clj-double'>0.4946039811847868</span>","value":"0.4946039811847868"},{"type":"html","content":"<span class='clj-double'>0.24257206654647998</span>","value":"0.24257206654647998"},{"type":"html","content":"<span class='clj-double'>0.025055633484052642</span>","value":"0.025055633484052642"}],"value":"(0.23776831878467936 0.4946039811847868 0.24257206654647998 0.025055633484052642)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.42246758878167495</span>","value":"0.42246758878167495"},{"type":"html","content":"<span class='clj-double'>0.35676800527690755</span>","value":"0.35676800527690755"},{"type":"html","content":"<span class='clj-double'>0.20986646941062562</span>","value":"0.20986646941062562"},{"type":"html","content":"<span class='clj-double'>0.010897936530791392</span>","value":"0.010897936530791392"}],"value":"(0.42246758878167495 0.35676800527690755 0.20986646941062562 0.010897936530791392)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.5943489281950304</span>","value":"0.5943489281950304"},{"type":"html","content":"<span class='clj-double'>0.3534632986000774</span>","value":"0.3534632986000774"},{"type":"html","content":"<span class='clj-double'>0.05153734825489197</span>","value":"0.05153734825489197"},{"type":"html","content":"<span class='clj-double'>6.50424949999459E-4</span>","value":"6.50424949999459E-4"}],"value":"(0.5943489281950304 0.3534632986000774 0.05153734825489197 6.50424949999459E-4)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.04112745379547697</span>","value":"0.04112745379547697"},{"type":"html","content":"<span class='clj-double'>0.37452319528981187</span>","value":"0.37452319528981187"},{"type":"html","content":"<span class='clj-double'>0.4897143959816161</span>","value":"0.4897143959816161"},{"type":"html","content":"<span class='clj-double'>0.09463495493309401</span>","value":"0.09463495493309401"}],"value":"(0.04112745379547697 0.37452319528981187 0.4897143959816161 0.09463495493309401)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.02231601245207923</span>","value":"0.02231601245207923"},{"type":"html","content":"<span class='clj-double'>0.15064418479021616</span>","value":"0.15064418479021616"},{"type":"html","content":"<span class='clj-double'>0.6825998068693793</span>","value":"0.6825998068693793"},{"type":"html","content":"<span class='clj-double'>0.14443999588832435</span>","value":"0.14443999588832435"}],"value":"(0.02231601245207923 0.15064418479021616 0.6825998068693793 0.14443999588832435)"}],"value":"((0.7357906745871806 0.23121408137183 0.030878530568161942 0.002116713472826875) (0.23776831878467936 0.4946039811847868 0.24257206654647998 0.025055633484052642) (0.42246758878167495 0.35676800527690755 0.20986646941062562 0.010897936530791392) (0.5943489281950304 0.3534632986000774 0.05153734825489197 6.50424949999459E-4) (0.04112745379547697 0.37452319528981187 0.4897143959816161 0.09463495493309401) (0.02231601245207923 0.15064418479021616 0.6825998068693793 0.14443999588832435))"}],"value":"[[[[[[[[[#'template/one-run,#'template/aggregate-means],#'template/square],#'template/sq-list],#'template/sq-dist-w],#'template/sum-sq-dist-per-run],#'template/std-dev],#'template/k-runs],#'template/meanAndDeviation],((0.7357906745871806 0.23121408137183 0.030878530568161942 0.002116713472826875) (0.23776831878467936 0.4946039811847868 0.24257206654647998 0.025055633484052642) (0.42246758878167495 0.35676800527690755 0.20986646941062562 0.010897936530791392) (0.5943489281950304 0.3534632986000774 0.05153734825489197 6.50424949999459E-4) (0.04112745379547697 0.37452319528981187 0.4897143959816161 0.09463495493309401) (0.02231601245207923 0.15064418479021616 0.6825998068693793 0.14443999588832435))]"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.13789813135836212</span>","value":"0.13789813135836212"},{"type":"html","content":"<span class='clj-double'>0.13496926521675395</span>","value":"0.13496926521675395"},{"type":"html","content":"<span class='clj-double'>0.0445900679142249</span>","value":"0.0445900679142249"},{"type":"html","content":"<span class='clj-double'>0.012154157625093559</span>","value":"0.012154157625093559"}],"value":"(0.13789813135836212 0.13496926521675395 0.0445900679142249 0.012154157625093559)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.12066733167840601</span>","value":"0.12066733167840601"},{"type":"html","content":"<span class='clj-double'>0.14805748702484578</span>","value":"0.14805748702484578"},{"type":"html","content":"<span class='clj-double'>0.13058458300185147</span>","value":"0.13058458300185147"},{"type":"html","content":"<span class='clj-double'>0.04609163141124757</span>","value":"0.04609163141124757"}],"value":"(0.12066733167840601 0.14805748702484578 0.13058458300185147 0.04609163141124757)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.149456578260434</span>","value":"0.149456578260434"},{"type":"html","content":"<span class='clj-double'>0.1374793760576716</span>","value":"0.1374793760576716"},{"type":"html","content":"<span class='clj-double'>0.11642492009627949</span>","value":"0.11642492009627949"},{"type":"html","content":"<span class='clj-double'>0.027801449625269755</span>","value":"0.027801449625269755"}],"value":"(0.149456578260434 0.1374793760576716 0.11642492009627949 0.027801449625269755)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.1567181273579154</span>","value":"0.1567181273579154"},{"type":"html","content":"<span class='clj-double'>0.15810378749772352</span>","value":"0.15810378749772352"},{"type":"html","content":"<span class='clj-double'>0.07342447852041392</span>","value":"0.07342447852041392"},{"type":"html","content":"<span class='clj-double'>0.0062719492014195145</span>","value":"0.0062719492014195145"}],"value":"(0.1567181273579154 0.15810378749772352 0.07342447852041392 0.0062719492014195145)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.04225420970184647</span>","value":"0.04225420970184647"},{"type":"html","content":"<span class='clj-double'>0.1368187179482514</span>","value":"0.1368187179482514"},{"type":"html","content":"<span class='clj-double'>0.1485901687516164</span>","value":"0.1485901687516164"},{"type":"html","content":"<span class='clj-double'>0.08978006454627499</span>","value":"0.08978006454627499"}],"value":"(0.04225420970184647 0.1368187179482514 0.1485901687516164 0.08978006454627499)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.041081786824385375</span>","value":"0.041081786824385375"},{"type":"html","content":"<span class='clj-double'>0.09373105006121468</span>","value":"0.09373105006121468"},{"type":"html","content":"<span class='clj-double'>0.1373459222392308</span>","value":"0.1373459222392308"},{"type":"html","content":"<span class='clj-double'>0.11499393300707485</span>","value":"0.11499393300707485"}],"value":"(0.041081786824385375 0.09373105006121468 0.1373459222392308 0.11499393300707485)"}],"value":"((0.13789813135836212 0.13496926521675395 0.0445900679142249 0.012154157625093559) (0.12066733167840601 0.14805748702484578 0.13058458300185147 0.04609163141124757) (0.149456578260434 0.1374793760576716 0.11642492009627949 0.027801449625269755) (0.1567181273579154 0.15810378749772352 0.07342447852041392 0.0062719492014195145) (0.04225420970184647 0.1368187179482514 0.1485901687516164 0.08978006454627499) (0.041081786824385375 0.09373105006121468 0.1373459222392308 0.11499393300707485))"}],"value":"[[[[[[[[[[#'template/one-run,#'template/aggregate-means],#'template/square],#'template/sq-list],#'template/sq-dist-w],#'template/sum-sq-dist-per-run],#'template/std-dev],#'template/k-runs],#'template/meanAndDeviation],((0.7357906745871806 0.23121408137183 0.030878530568161942 0.002116713472826875) (0.23776831878467936 0.4946039811847868 0.24257206654647998 0.025055633484052642) (0.42246758878167495 0.35676800527690755 0.20986646941062562 0.010897936530791392) (0.5943489281950304 0.3534632986000774 0.05153734825489197 6.50424949999459E-4) (0.04112745379547697 0.37452319528981187 0.4897143959816161 0.09463495493309401) (0.02231601245207923 0.15064418479021616 0.6825998068693793 0.14443999588832435))],((0.13789813135836212 0.13496926521675395 0.0445900679142249 0.012154157625093559) (0.12066733167840601 0.14805748702484578 0.13058458300185147 0.04609163141124757) (0.149456578260434 0.1374793760576716 0.11642492009627949 0.027801449625269755) (0.1567181273579154 0.15810378749772352 0.07342447852041392 0.0062719492014195145) (0.04225420970184647 0.1368187179482514 0.1485901687516164 0.08978006454627499) (0.041081786824385375 0.09373105006121468 0.1373459222392308 0.11499393300707485))]"}
;; <=

;; **
;;; On running the above code, we get:
;;; a mean of 
;;; <br />
;;; ((0.7357906745871806 0.23121408137183 0.030878530568161942 0.002116713472826875) <br />
;;; (0.23776831878467936 0.4946039811847868 0.24257206654647998 0.025055633484052642) <br />
;;; (0.42246758878167495 0.35676800527690755 0.20986646941062562 0.010897936530791392) <br />
;;; (0.5943489281950304 0.3534632986000774 0.05153734825489197 6.50424949999459E-4) <br />
;;; (0.04112745379547697 0.37452319528981187 0.4897143959816161 0.09463495493309401) <br />
;;; (0.02231601245207923 0.15064418479021616 0.6825998068693793 0.14443999588832435))
;;; 
;;; and a standard deviation of:<br />
;;; ((0.13789813135836212 0.13496926521675395 0.0445900679142249 0.012154157625093559) <br />
;;; (0.12066733167840601 0.14805748702484578 0.13058458300185147 0.04609163141124757) <br />
;;; (0.149456578260434 0.1374793760576716 0.11642492009627949 0.027801449625269755) <br />
;;; (0.1567181273579154 0.15810378749772352 0.07342447852041392 0.0062719492014195145) <br />
;;; (0.04225420970184647 0.1368187179482514 0.1485901687516164 0.08978006454627499) <br />
;;; (0.041081786824385375 0.09373105006121468 0.1373459222392308 0.11499393300707485))
;; **

;; @@

;; @@
