(ns recursion)
;; (clojure.repl/doc first)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;; (product [])        ;=> 1  ; special case
;; (product [1 2 3])   ;=> 6
;; (product [1 2 3 4]) ;=> 24
;; (product [0 1 2])   ;=> 0
;; (product #{2 3 4})  ;=> 24 ; works for sets too!

;; general template for linear recursion
;; (defn eats-coll [coll]
;;   (if (empty? coll)
;;     ...
;;     (... (first coll) ... (eats-coll (rest coll)))))
(second '(:f))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))



;; (singleton? [1])     ;=> true
;; (singleton? #{2})    ;=> true
;; (singleton? [])      ;=> false
;; (singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (cond
     (empty? coll) nil
     (singleton? coll) (first coll)
     :else (my-last (rest coll))))

;; (my-last [])      ;=> nil
;; (my-last [1 2 3]) ;=> 3
;; (my-last [2 5])   ;=> 5

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

;; (max-element [2 4 1 4]) ;=> 4
;; (max-element [2])       ;=> 2
;; (max-element [])        ;=> nil

;; it's okay to use count for this one
(defn seq-max
  "returns the sequence that is longer,
  if a tie, returns second sequence"
  [seq-1 seq-2]

  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

;; (seq-max [1] [1 2])   ;=> [1 2]
;; (seq-max [1 2] [3 4]) ;=> [3 4]

;; god this would be so easy with reduce
;; this looks like the ones we did before though
;; same format, and problem size is reduced by 1
;; each time
(defn longest-sequence
  "Takes a sequence of sequences as a
  parameter and returns the longest one."
  [a-seq]
  (cond
     (empty? a-seq) nil
     (singleton? a-seq) (first a-seq)
     :else (seq-max (first a-seq)
                    (longest-sequence (rest a-seq)))))

;; (longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;; (longest-sequence [[1 2]])            ;=> [1 2]
;; (longest-sequence [])                 ;=> nil


;; (clojure.repl/doc map)

;; I want to put this into the let loop
;; but I don't know how to (my-filter pred? (rest a-seq))
(defn my-filter [pred? a-seq]

  (let [first-thing (first a-seq)
        recursive-step '(my-filter pred? (rest a-seq))]
    (cond
      (empty? a-seq) '()
      (pred? first-thing) (cons first-thing
                                (my-filter pred? (rest a-seq)))
      :else (my-filter pred? (rest a-seq)))))


;; (my-filter odd? [1 2 3 4]) ;=> (1 3)
;; (my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;; (my-filter even? [1 3 5 7]) ;=> ()


(defn sequence-contains? [elem a-seq]
  (cond
     (empty? a-seq) false
     (not= (first a-seq) elem)
       (sequence-contains? elem (rest a-seq))
     :else true))

;; (sequence-contains? 3 [1 2 3]) ;=> true
;; (sequence-contains? 3 [4 7 9]) ;=> false
;; (sequence-contains? :pony [])  ;=> false

;; this is sorta magic
(defn my-take-while [pred? a-seq]
  (cond
     (empty? a-seq) '()
     (pred? (first a-seq)) (cons (first a-seq)
                                 (my-take-while pred? (rest a-seq)))
     :else '()))

;; (my-take-while odd? [1 2 3 4])  ;=> (1)
;; (my-take-while odd? [1 3 4 5])  ;=> (1 3)
;; ;; the case below is very odd,
;; ;; it should return (4) but even the exercise says it returns (4)
;; ;; this is because it's a take "while", first is false so terminates
;; (my-take-while even? [1 3 4 5]) ;=> ()
;; (my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (cond
     (empty? a-seq) '()
     (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
     :else a-seq))

;; (my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;; (my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;; (my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;; (my-drop-while odd? [])         ;=> ()

(defn seq= [a-seq b-seq]
  (cond
     (empty? a-seq) (empty? b-seq)
     (empty? b-seq) (empty? a-seq)
     (= (first a-seq) (first b-seq))
       (seq= (rest a-seq) (rest b-seq))
     :else false))

;;  (seq= [1 2 4] '(1 2 4)) ;=> true
;; (seq= [] []) ;=> true
;; (seq= [1 2 nil] [1 2]) ;=> false
;; (seq= [1 4 2] [1 2 4]) ;=> false
;; (seq= [1 2 3] [1 2 3 4]) ;=> false
;; (seq= [1 3 5] []) ;=> false
;; (seq= [1 2 4] '(1 2 4))  ;=> true
;; (seq= [1 2 3] [1 2 3 4]) ;=> false
;; (seq= [1 3 5] [])        ;=> false

;; ah this is so cool! it's like im implementing
;; the core
(defn my-map [f seq-1 seq-2]
  (cond
     (or (empty? seq-1)
         (empty? seq-2))
       '()
     :else
       (cons (f (first seq-1) (first seq-2))
             (my-map f (rest seq-1) (rest seq-2)))))


;; (my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
;; (my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
;; (my-map + [1 2 3] [])        ;=> ()


;; general template for recursion over natural nums
;; (defn eats-numbers [n]
;;   (if (zero? n)
;;     ...
;;     (... n ... (eats-numbers (dec n)))))
(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

;; non linear recursion time

;; sigh do it the inefficient way
;; blow stack so hard
;; shape of this computation is a tree

(defn fib [n]
  (cond
     (zero? n) 0
     (= n 1) 1
     :else (+
              (fib (- n 2))
              (fib (- n 1)))))

;; (fib 0) ;=> 0
;; (fib 1) ;=> 1
;; (fib 2) ;=> 1
;; (fib 3) ;=> 2
;; (fib 4) ;=> 3
;; (fib 5) ;=> 5
;; (fib 6) ;=> 8
;; (fib 10) ;=> 55

(defn my-repeat [how-many-times what-to-repeat]
  (cond
     (< how-many-times 1) '()
     :else (cons what-to-repeat
                 (my-repeat (dec how-many-times)
                            what-to-repeat))))

;; (my-repeat 2 :a)    ;=> (:a :a)
;; (my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;; (my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

;; (my-range 0)  ;=> ()
;; (my-range 1)  ;=> (0)
;; (my-range 2)  ;=> (1 0)
;; (my-range 3)  ;=> (2 1 0)

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq
          (tails (rest a-seq)))))

;; (tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())

;; dammnnn this is ugly
(defn inits [a-seq]
  (cond
     (empty? a-seq) '(())
     :else (cons a-seq
                 (inits (reverse (rest(reverse a-seq)))))))

(take 2 [1 2 3])

(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
; You can output the tails and inits in any order you like.
(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))

(defn rotations-helper [a-seq rotations]
  (if (zero? rotations)
    '()
     (cons a-seq
           (rotations-helper (cons (last a-seq)
                                   (butlast a-seq))
                             (dec rotations)))))

;; had to use a helper
;; since it was really difficult
;; to break the recursion, with the restrictions imposed

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper a-seq (count a-seq))))


;; (rotations [])        ;=> (())
;; (rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
;; (rotations [:a :b])   ;=> ((:a :b) (:b :a))
;; ; The order of rotations does not matter.
;; (rotations [:a :b])   ;=> ((:b :a) (:a :b))
;; (rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
;; (count (rotations [6 5 8 9 2])) ;=> 5

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a-key (first a-seq)
          new-freqs (assoc freqs a-key (inc (get freqs a-key 0)))]
      (my-frequencies-helper new-freqs
                             (rest a-seq)))))

;; (get {:a :b, 1 2} 2 :default)
;; (conj {} [:a 1] [:b 3])

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)))

;; (my-frequencies []) ;=> {}
;; (my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

;; use concat and repeat...
(defn un-frequencies
  "takes a map from my-frequencies
  and outputs a sequence with said frequencies"
  [a-map]
  (if (empty? a-map)
    '()
    (let [repeat-this (first (first a-map))
          times (second (first a-map))]

      (concat (repeat times repeat-this)
              (un-frequencies (rest a-map))))))


(repeat 2 "3")

;; (un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
;; (un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
;; (my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

;; (my-take 2 [1 2 3 4]) ;=> (1 2)
;; (my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (cond
      (empty? coll) '()
      (zero? n) coll
      :else (my-drop (dec n) (rest coll))))

;; (my-drop 2 [1 2 3 4]) ;=> (3 4)
;; (my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
     [(my-take half a-seq) (my-drop half a-seq)]))

;; (halve [1 2 3 4])   ;=> [(1 2) (3 4)]
;; (halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;; (halve [1])         ;=> [() (1)]

(defn seq-merge [a-seq b-seq]
  (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (> (first a-seq) (first b-seq))
       (cons (first b-seq)
          (seq-merge a-seq (rest b-seq)))
     :else ;; first element of a-seq is <= first element of b-seq
       (cons (first a-seq)
             (seq-merge (rest a-seq) b-seq))))


;; (seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;; (seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
  (if (or (= 1 (count a-seq)) (zero? (count a-seq)))
    a-seq
    (let [half (int (/ (count a-seq) 2))]
      (seq-merge (merge-sort (first (halve a-seq)))
                 (merge-sort (second (halve a-seq)))))))


;; so cool!
;; (merge-sort [])                 ;=> ()
;; (merge-sort [1 2 3])            ;=> (1 2 3)
;; (merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

