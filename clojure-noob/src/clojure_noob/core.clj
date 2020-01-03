(ns clojure-noob.core
  (:gen-class))

;;;; Map Summary

;; 精妙啊，精妙
;; 这里key-fn是用来聚类的，f是用来累计的函数，init是起始值，coll是数据库
;; assoc是可以更新map中的值的
(defn reduce-by 
  [key-fn f init coll]
  (reduce (fn [summaries x] 
            (let [k (key-fn x)]
              (assoc summaries k (f (summaries k init) x))))
          {} coll))

;; assoc-in和get-in这两个神奇的函数
(defn reduce-by-in
  [keys-fn f init coll]
  (reduce (fn [summaries x]
            (let [ks (keys-fn x)]
              (assoc-in summaries ks (f (get-in summaries ks init) x))))
          {} coll))

(def orders
  [{:product "Clock", :customer "Wile Coyote", :qty 6, :total 300}
   {:product "Dynamite", :customer "Wile Coyote", :qty 20, :total 5000}
   {:product "Shotgun", :customer "Elmer Fudd", :qty 2, :total 800}
   {:product "Shells", :customer "Elmer Fudd", :qty 4, :total 100}
   {:product "Hole", :customer "Wile Coyote", :qty 1, :total 1000}
   {:product "Anvil", :customer "Elmer Fudd", :qty 2, :total 300}
   {:product "Anvil", :customer "Wile Coyote", :qty 6, :total 900}])

;; order totals by customer
(reduce-by :customer #(+ %1 (:total %2)) 0 orders)
;; customers for each product
(reduce-by :product #(conj %1 (:customer %2)) #{} orders)
;; all orders by customer, and then by product
(reduce-by (juxt :customer :product) #(+ %1 (:total %2)) 0 orders)
(reduce-by-in (juxt :customer :product) #(+ %1 (:total %2)) 0 orders)
;; same result as previous function
(def flat-breakup
  {["Wile Coyote" "Anvil"] 900,
   ["Elmer Fudd" "Anvil"] 300, 
   ["Wile Coyote" "Hole"] 1000, 
   ["Elmer Fudd" "Shells"] 100, 
   ["Elmer Fudd" "Shotgun"] 800, 
   ["Wile Coyote" "Dynamite"] 5000,
   ["Wile Coyote" "Clock"] 300})
(reduce #(apply assoc-in %1 %2) {} flat-breakup)



