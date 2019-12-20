(ns ski-calculus.ski-test
  (:require [clojure.test :refer :all]
            [ski-calculus.settings :as settings]
            [ski-calculus.ski :refer :all]))

(deftest common-test
  (doall
   (for [basis [:ski :sk]
         fix-variant (range 2)]
     (binding [settings/settings {:basis basis
                                  :fix-variant fix-variant}]
       (require '[ski-calculus.ski :refer :all] :reload ;; :reload-all ;; :verbose
                )
       (prn settings/settings)

       (testing "different realizations of I"
         (is (= (case basis
                  :ski 'I
                  :sk '((S K) K))
                (show I)))
         (is (= 'I (show- I)))
         (is (= 42 (eval (to-ski [I 42])))))

       (testing "pair fst snd"
         (is (= 1 (raw-to-int [fst [pair one two]])))
         (is (= 2 (raw-to-int [snd [pair one two]]))))

       (testing "church numerals, ariphmetic"
         (is (= 55 (church-to-int (church-num 55))))

         (is (= 8 (raw-to-int [add (church-num 5) (church-num 3)])))
         (is (= 15 (raw-to-int [mul (church-num 5) (church-num 3)])))
         (is (= 125 (raw-to-int [exp (church-num 5) (church-num 3)])))
         (is (= 54 (raw-to-int [pred (church-num 55)])))
         (is (= 2 (raw-to-int [sub (church-num 5) (church-num 3)]))))

       (testing "simple functions"
         (is (= 61 (raw-to-int [pif (church-num 5) (church-num 6)]))))

       (testing "boolean logic - lazy if by second argument"
         (is (= 1 (eval (to-ski [if* true*  1 2]))))
         (is (= 2 (eval (to-ski [if* false* 1 (lam [:z] 2)])))))

       (testing "fixed point combinator"
         (is (= fix (if (= 0 fix-variant) Z-0 Z-1))))

       (testing "factorial"
         (is (= 720 (fix-to-int [fact (church-num 6)]))))

       (testing "fibonacci"
         (is (= 55 (fix-to-int [fib (church-num 10)])))
         (is (= 6765 (fix-to-int [fib-iter zero one (church-num 20)]))))

       (testing "foo-bar"
         (is (= 91 (fix-to-int [foo (church-num 6)])))
         (is (= 91 (fix-to-int [bar (church-num 6) zero]))))

       (testing "coin by id"
         (is (= [1 5 10 25 50]
                (mapv #(raw-to-int [coin-by-id-classic (church-num %)])
                      [1 2 3 4 5])))
         (is (= [1 4 9 16 25]
                (mapv #(raw-to-int [coin-by-id (church-num %)])
                      [1 2 3 4 5]))))

       (testing "coin change"
         (is (= 97 (fix-to-int [cc (church-num 50) (church-num 5)]))))

       (testing "height"
         (is (= 105 (fix-to-int [height two (church-num 14)]))))

       (testing "without 2 zeroes"
         (is (= 210 (fix-to-int [without-2-zeroes (church-num 4) (church-num 9) one]))))))))

(comment
  (run-tests)
;;
  )
