# SKI calculus

Simple SKI-based combinator calculus interpreter, written in Clojure.

## Overview

This project presents an inner representation of SKI-based combinator terms, DSL for writing new terms (with ability of creation new closed-form lambda abstractions), translating it to SKI-based combinator form and functions for evaluation and pretty user representation of this terms.

## Command line examples

Run REPL in terminal

    lein repl

load namespace ski and evaluate forms

    ski=> (def B (to-ski 'B [S [K S] K]))
    #'ski/B
    ski=> (show B)
    ((S (K S)) K)
    ski=> (def C (to-ski 'C [S [B B S] [K K]]))
    #'ski/C
    ski=> (show C)
    ((S ((((S (K S)) K) ((S (K S)) K)) S)) (K K))
    ski=> (def f3 (to-ski [S B [S B [S B [K I]]]]))
    #'ski/f3
    ski=> (show- f3)
    ((S B) ((S B) ((S B) (K I))))
    ski=> (show f3)
    ((S ((S (K S)) K)) ((S ((S (K S)) K)) ((S ((S (K S)) K)) (K I))))
    ski=> (def b (lam [:x :y :z] [:x [:y :z]]))
    #'ski/b
    ski=> (-> b to-ski showp-)
    ((S
      ((S (K S))
       ((S ((S (K S)) ((S (K K)) (K S))))
        ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I)))))
     ((S
       ((S (K S))
        ((S ((S (K S)) ((S (K K)) (K S))))
         ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))))
      ((S (K K)) (K I))))
    
    ski=> (raw-to-int [add (church-num 5) (church-num 3)])
    8
    ski=> (raw-to-int [mul (church-num 5) (church-num 3)])
    15
    ski=> (raw-to-int [exp (church-num 5) (church-num 3)])
    125
    ski=> (raw-to-int [sub (church-num 5) (church-num 3)])
    2
    ski=> (def fact (to-ski
     #_=>            (lam [:f :x]
     #_=>                 [if* [is-zero :x]
     #_=>                  one
     #_=>                  (lam [:z] [mul :x [:f [pred :x]]])])))
    #'ski/fact
    ski=> (showp- fact)
    ((S
      ((S (K S))
       ((S
         ((S (K S))
          ((S ((S (K S)) ((S (K K)) (K I))))
           ((S ((S (K S)) ((S (K K)) (K is-zero-lazy)))) (K I)))))
        ((S (K K)) (K one)))))
     ((S
       ((S (K S))
        ((S ((S (K S)) ((S (K K)) (K S))))
         ((S
           ((S (K S))
            ((S ((S (K S)) ((S (K K)) (K S))))
             ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) (K mul))))))
          ((S ((S (K S)) ((S (K K)) (K K)))) (K I))))))
      ((S
        ((S (K S))
         ((S ((S (K S)) ((S (K K)) (K S))))
          ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I)))))
       ((S
         ((S (K S))
          ((S ((S (K S)) ((S (K K)) (K S))))
           ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) (K pred))))))
        ((S ((S (K S)) ((S (K K)) (K K)))) (K I))))))
    
    ski=> (fix-to-int [fact (church-num 6)])
    720
    ski=> (mapv #(raw-to-int [coin-by-id (church-num %)]) [1 2 3 4 5])
    [1 5 10 25 50]
    ski=> (fix-to-int [cc (church-num 50) (church-num 5)])
    97
    ski=> (fix-to-int [height two (church-num 14)])
    105


## License

Copyright © 2018

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
