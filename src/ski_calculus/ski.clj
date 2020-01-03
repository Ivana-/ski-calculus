(ns ski-calculus.ski
  (:require [clojure.pprint :as pprint]
            [ski-calculus.settings :as settings]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basics, show / to-ski
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn show-core [base-level? t]
  (letfn [(go [t] (or (when-not base-level? (:name (meta t)))
                      (:base-name (meta t))
                      (cond
                        (fn? t) 'fn
                        (seq? t) (map go t)
                        (vector? t) (mapv go t)
                        (map? t) (reduce (fn [acc [k v]] (assoc acc k (go v))) {} t)
                        :else t)))]
    (go t)))

(defn show  [t] (show-core true t))
(defn show- [t] (show-core false t))

(defn showp [t] ; (binding [pprint/*print-right-margin* 100]
  (pprint/pprint (show t))
  (symbol " "))
(defn showp- [t]
  (pprint/pprint (show- t))
  (symbol " "))







;; Sxyz = xz(yz)     ;; Connector

;; Kxy = x           ;; Cancellator

;; Ix = x

;; SKKx = Kx(Kx) = x = I

;; SKSx = Kx(Sx) = x = I

;; SK_x = Kx(_x) = x = I









;; transform (Var x) = Var x
;; transform (App x y) = App (transform x) (transform y)
;; transform (Lam x (Var y))
;; | x == y           = i
;; | otherwise        = App k (Var y)
;; transform (Lam x (l@(Lam _ _))) = transform (Lam x (transform l))
;; transform (Lam x (App e1 e2)) = App (App s (transform (Lam x e1))) (transform (Lam x e2))

(declare S)
(declare K)
(declare I)

(defn to-ski
  ([meta-name t]
   (let [tr (fn tr [t]
              (cond
                (vector? t) (mapv tr (reduce vector t))
                (map? t) (let [[x v] (first t)]
                           (cond
                             (vector? v) (let [[a b] (reduce vector v)] [[S (tr {x a})] (tr {x b})])
                             (map? v) (tr {x (tr v)})
                             (= x v) I ;; (tr [S K K])
                             :else [K v]))
                :else t))
         tl (fn tl [t] (if (vector? t) (map tl t) t))]
     (cond-> (tl (tr t))
       meta-name (with-meta {:name meta-name}))))
  ([t] (to-ski nil t)))

;; Sxyz = xz(yz)
;; Kxy = x
;; Ix = x = SKKx

(def S ^{:base-name 'S} (fn [x] (fn [y] (fn [z] ((x z) (y z))))))  ;; Connector

(def K ^{:base-name 'K} (fn [x] (fn [y] x)))                       ;; Cancellator

(def I (if (= :ski (:basis settings/settings))
         ^{:base-name 'I} (fn [x] x)
         (with-meta (seq [(seq [S K]) K]) {:name 'I})))

(comment

  (show I)

  (-> [S K K]
      to-ski)

  (-> [S K K]
      to-ski
      show)

  (let [f (-> [S K K]
              to-ski
              eval)]
    (f "zazaza"))
  ;;
  )


(def B (to-ski 'B [S [K S] K]))        ;; Compositor

(def C (to-ski 'C [S [B B S] [K K]]))  ;; Permutator

(comment
  (show B)
  (show- B)
  (show C)
  (show- C)
  ;;
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Church numerals, ariphmetics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn church-num
  ([meta-name n] (letfn [(go [n] (if (= 0 n) [K I] [S B (go (dec n))]))] (to-ski meta-name (go n))))
  ([n] (church-num nil n)))

(defn church-to-int [x] (((eval x) inc) 0))

(defn church-to-str [x] (str "{" (((eval x) #(str "|" %)) "}")))

(defn raw-to-int [v] (-> v to-ski church-to-int))

(comment
  (def f3 (to-ski [S B [S B [S B [K I]]]]))
  (show- f3)
  (show f3)
  (show- (church-num 5))
  (show (church-num 5))
  (church-to-int f3)
  (church-to-str f3)
  ;;
  )


(def add (to-ski 'add [C I [S B]]))
(def mul (to-ski 'mul B))
(def exp (to-ski 'exp [C I]))

(comment
  (def x (to-ski [add (church-num 5) (church-num 3)]))
  (show- x)
  (church-to-int x)

  (def x (to-ski [mul (church-num 5) (church-num 3)]))
  (show- x)
  (church-to-int x)

  (def x (to-ski [exp (church-num 5) (church-num 3)]))
  (show- x)
  (church-to-int x)
  (church-to-str x)
  ;;
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lambda syntax - convert into SKI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn lam [args body] {(first args) (if (empty? (rest args)) body (lam (rest args) body))})

(comment
  (def b (lam [:x :y :z] [:x [:y :z]]))
  b
  (-> b to-ski showp-)
  ;;
  )

(def pif (to-ski (lam [:x :y] [add [mul :x :x] [mul :y :y]])))

(comment
  (showp pif)
  (-> [pif (church-num 5) (church-num 6)] to-ski showp)
  (-> [pif (church-num 5) (church-num 6)] to-ski showp-)
  (raw-to-int [pif (church-num 5) (church-num 6)])
  ;;
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean logic, lazy 'if' call by second argument!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(comment
  (defn lazy-if [p by-true by-false] (if p by-true by-false))
  (lazy-if true 1 2)
  (lazy-if false 1 2)

  (lazy-if true 1 (/ 1 0)) ; Execution error!

  ;; ???????

  ;;
  )


(def true* K) ;; (to-ski (lam [:x :y] :x))
(def false* (to-ski 'lazy-false (lam [:x :y] [:y I]))) ;; convention, that y is lambda!
(def if* I) ;; (show- (to-ski (lam [:p :t :f] [:p :t :f])))

(comment
  (show false*)
  (eval (to-ski [if* true*   1 2]))
  (eval (to-ski [if* false*  1 2])) ; Execution error
  (eval (to-ski [if* false* 1 (lam [:z] 2)]))
  ;;
  )


(def zero (church-num 'zero 0))
(def one  (church-num 'one 1))
(def two  (church-num 'two 2))

(def is-zero (to-ski 'is-zero-lazy (lam [:n] [:n (lam [:c] false*) true*])))

(comment
  (show is-zero)
  (def tst (to-ski (lam [:x] [if* [is-zero :x] one (lam [:z] two)])))
  (show- tst)
  (showp tst)
  (raw-to-int [tst zero])
  (raw-to-int [tst (church-num 33)])
  ;;
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pairs, pred
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def pair (to-ski 'pair [B C [C I]]))
(def fst (to-ski 'fst [C I K]))
(def snd (to-ski 'snd [C I [K I]]))

(comment
  (raw-to-int [fst [pair one two]])
  (raw-to-int [snd [pair one two]])
  ;;
  )

(def pred (to-ski 'pred (lam [:n :s :z] [snd [:n (lam [:p] [pair [:s [fst :p]] [fst :p]]) [pair :z :z]]])))

(comment
  (showp pred)
  (raw-to-int [pred (church-num 55)])
  ;;
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixed point conbinator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(def fix (to-ski (lam [:f] [(lam [:x] [:f [:x :x]]) (lam [:x] [:f [:x :x]])])))
;;(def fix (to-ski [S S K [S [K [S S [S [S S K]]]]]]))
;;(def fix (to-ski [S [K [S I I]] [S [S [K S] K] [K [S I I]]]]))
;;(def fix (to-ski [S S K [S [K [S S [S [S S K]]]] K]]))

;; Z - strict Y
(def Z-0 (to-ski 'Z (lam [:f] [(lam [:x] [:f (lam [:v] [:x :x :v])])
                               (lam [:x] [:f (lam [:v] [:x :x :v])])])))
                              
(def Z-1 (to-ski 'Z (lam [:f] [S I I (lam [:x] [:f (lam [:v] [:x :x :v])])])))

(def fix (if (= 0 (:fix-variant settings/settings)) Z-0 Z-1))

(comment
  (show (to-ski (lam [:f] [:f :f])))
  (showp Z-0)
  (showp Z-1)
  ;;
  )

(defn fix-to-int [v] (-> (into [fix] v) to-ski church-to-int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive function examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Factorial
(def fact (to-ski (lam [:f :x] [if* [is-zero :x] one (lam [:z] [mul :x [:f [pred :x]]])])))

(comment
  (showp- fact)
  (showp fact)
  (fix-to-int [fact (church-num 6)])
  ;;
  )


;; Fibonacci - exponential recursion
(def fib (to-ski (lam [:f :x] [if* [is-zero :x] zero
                               (lam [:z1] [if* [is-zero [pred :x]] one
                                           (lam [:z2] [add [:f [pred :x]] [:f [pred [pred :x]]]])])])))

(comment
  (showp- fib)
  (showp fib)
  (fix-to-int [fib (church-num 10)])
  ;;
  )


;; Fibonacci - linear recursion
(def fib-iter (to-ski (lam [:f :a :b :x] [if* [is-zero :x] :a (lam [:z] [:f :b [add :a :b] [pred :x]])])))

(comment
  (showp- fib-iter)
  (showp fib-iter)
  (fix-to-int [fib-iter zero one (church-num 20)])
  ;;
  )


;; Sum of squares - non tail recursive
(def foo (to-ski (lam [:f :x] [if* [is-zero :x] zero (lam [:z] [add [mul :x :x] [:f [pred :x]]])])))

(comment
  (showp- foo)
  (showp foo)
  (fix-to-int [foo (church-num 6)])
  ;;
  )

;; Sum of squares - tail recursive
(def bar (to-ski (lam [:f :x :a] [if* [is-zero :x] :a (lam [:z] [:f [pred :x] [add [mul :x :x] :a]])])))

(comment
  (showp- bar)
  (showp bar)
  (fix-to-int [bar (church-num 6) zero])
  ;;
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coin change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Substitution
(def sub (to-ski 'sub (lam [:n :m] [:m pred :n])))

(comment
  (showp sub)
  (raw-to-int [sub (church-num 16) (church-num 4)])
  ;;
  )


;; strict predicate
(def false-strict (to-ski 'strict-false [K I]))

(def is-zero-strict (to-ski 'is-zero-strict (lam [:n] [:n (lam [:c] false-strict) true*])))


;; coin values
(def coin-by-id-classic
  (to-ski (lam [:x] [#_if* [is-zero-strict [pred :x]] (church-num 1)
                     [#_if* [is-zero-strict [sub :x (church-num 2)]] (church-num 5)
                      [#_if* [is-zero-strict [sub :x (church-num 3)]] (church-num 10)
                       [#_if* [is-zero-strict [sub :x (church-num 4)]]
                        [exp (church-num 5) two]
                        [mul [exp (church-num 5) two] two]]]]])))
(comment
  (showp- coin-by-id-classic)
  (showp coin-by-id-classic)
  (mapv #(raw-to-int [coin-by-id-classic (church-num %)]) [1 2 3 4 5])
  )

;; coin change

;; (define (cc amount kinds-of-coins)
;;   (cond ((= amount 0) 1)
;;         ((or (< amount 0) (= kinds-of-coins 0)) 0)
;;         (else (+ (cc amount (- kinds-of-coins 1)) (cc (- amount (denom kinds-of-coins)) kinds-of-coins)))))

; (def coin-by-id coin-by-id-classic) ; Syntax error: Method code too large!
(def coin-by-id (to-ski (lam [:x] [exp :x two])))

(def cc (to-ski
         (lam [:f :a :k]
              [#_if* [is-zero-strict :a] one
               [#_if* [is-zero :k] zero
                (lam [:z2]
                     [add [:f :a [pred :k]]
                      [#_if* [is-zero [sub [add :a one] [coin-by-id :k]]] zero
                       (lam [:z3] [:f [sub :a [coin-by-id :k]] :k])]])]])))

(comment
  (showp- cc)
  (fix-to-int [cc (church-num 50) (church-num 5)])
  ;;
  )


;; Eggs crash test

;; height (n, m) | m <= 0 || n <= 0 = Left 0
;;               | otherwise        = Right ([(n, m-1), (n-1, m-1)], (+1).sum)

(def height (to-ski (lam [:f :n :m] [[is-zero-strict :n] zero
                                     [[is-zero :m] zero
                                      (lam [:z] [add one [add [:f :n [pred :m]] [:f [pred :n] [pred :m]]]])]])))

(comment
  (showp- height)
  (fix-to-int [height two (church-num 14)])
  ;;
  )


;; Without 2 zeroes

;; (defn f [z o v]
;;   (if
;;    (= 0 z o) 1
;;    (+ (if (or (= z 0) (= v 0)) 0 (f (dec z) o 0))
;;       (if (= o 0) 0 (f z (dec o) 1)))))

;; (f 2 2 1)
;; (f 1 1 1)
;; (f 1 3 1)
;; (f 2 4 1)
;; (f 4 9 1)

(def without-2-zeroes
  (to-ski (lam [:f :z :o :v]
               [[is-zero-strict [add :z :o]] one
                [add
                 [[is-zero [mul :z :v]] zero (lam [:x] [:f [pred :z] :o zero])]
                 [[is-zero :o] zero (lam [:x] [:f :z [pred :o] one])]]])))

(comment
  (showp without-2-zeroes)
  (fix-to-int [without-2-zeroes two two one])
  (fix-to-int [without-2-zeroes one one one])
  (fix-to-int [without-2-zeroes one (church-num 3) one])
  (fix-to-int [without-2-zeroes two (church-num 4) one])
  (fix-to-int [without-2-zeroes (church-num 4) (church-num 9) one])
  ;;
  )
