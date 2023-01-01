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

;; **
;;; Q2 b) i) Assumptions and explanations are listed on the written copy.
;; **

;; @@
(def stddev (Math/sqrt 2))

(defquery lateStudents []
  (let [samePerson (sample (flip 0.3))
        distance1 (sample (normal 20 stddev))
        distance2 (sample (normal 20 stddev))
         ]
    (observe (normal distance1 stddev) 16)
    (if samePerson
      (observe (normal distance1 stddev) 25)
      (observe (normal distance2 stddev) 25))
    samePerson
    ))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/stddev</span>","value":"#'template/stddev"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/lateStudents</span>","value":"#'template/lateStudents"}],"value":"[#'template/stddev,#'template/lateStudents]"}
;; <=

;; @@

;; @@
