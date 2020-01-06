(ns clojure-noob.chapter03)
(require '[clojure.pprint :as pp])
(require '[clojure.zip :as z])


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


;;;; Conway’s Game of Life
(defn empty-board
  "Creates a rectangular empty board of the specified width and height."
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  "Turns :on each of the cells specified as [y, x] coordinates." 
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on)) 
          board
          living-cells))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] 
    [(+ dx x) (+ dy y)]))

(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn indexed-step
  "Yields the next state of the board, using indices to determine neighbors, liveness, etc."
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board x 0 y 0]
      (cond
        (>= x w) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
        (let [new-liveness
              (case (count-neighbours board [x y])
                2 (get-in board [x y]) 
                3 :on
                nil)]
          (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

;; reduce这个抽象里包含了循环遍历
(defn indexed-step2 [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board x]
       (reduce
        (fn [new-board y]
          (let [new-liveness
                (case (count-neighbours board [x y])
                  2 (get-in board [x y]) 
                  3 :on
                  nil)]
            (assoc-in new-board [x y] new-liveness))) 
        new-board (range h)))
     board (range w))))

;; 书中的代码有错误，搞错了x和y的位置
(defn indexed-step3 [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board [x y]]
       (let [new-liveness
             (case (count-neighbours board [x y])
               2 (get-in board [x y]) 
               3 :on
               nil)]
         (assoc-in new-board [x y] new-liveness))) 
     board (for [x (range h) y (range w)] [x y]))))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))
(-> (iterate indexed-step glider) (nth 8) pp/pprint)
(-> (iterate indexed-step2 glider) (nth 8) pp/pprint)
(-> (iterate indexed-step3 glider) (nth 8) pp/pprint)

(defn window
  "Returns a lazy sequence of 3-item windows centered around each item of coll, padded as necessary with pad or nil."
  ([coll] (window nil coll)) 
  ([pad coll]
   (partition 3 1 (concat [pad] coll [pad]))))

(defn cell-block
  "Creates a sequences of 3x3 windows from a triple of 3 sequences." 
  [[left mid right]]
  (window (map vector left mid right)))

(defn liveness
  "Returns the liveness (nil or :on) of the center cell for the next step."
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block))) (if (= :on center) 1 0))
      2 center 
      3 :on 
      nil)))

(defn step-row
  "Yields the next state of the center row." 
  [rows-triple]
  (vec (map liveness (cell-block rows-triple))))

;; 用了一个多小时，才大概明白这个(repeate nil)的用法
;; 这个lazy的机制很牛逼啊，需要用的时候就有，自动填充，而且是在其他函数的计算中
;; 真的需要适应
(defn index-free-step
  "Yields the next state of the board."
  [board]
  (vec (map step-row (window (repeat nil) board))))

(= (nth (iterate indexed-step glider) 8) 
   (nth (iterate index-free-step glider) 8))

;; 函数是概念的表达，思路不同，函数不同
;; nb啊nb
(defn step
  "Yields the next state of the world"
  [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))] 
         loc)))

(->> (iterate step #{[2 0] [2 1] [2 2] [1 2] [0 1]}) 
     (drop 9)
     first
     (populate (empty-board 6 6)) 
     pp/pprint)

(defn stepper
  "Returns a step function for Life-like cell automata. neighbours takes a location and return a sequential collection of locations. survive? and birth? are predicates on the number of living neighbours."
  [neighbours birth? survive?] 
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells)) 
               :when (if (cells loc) (survive? n) (birth? n))]
           loc))))

;; 抽象是去掉隐藏在代码中的无关限制条件
;; Leak of Abastraction


;; Maze Wilson’s algorithm
(defn maze
  "Returns a random maze carved out of walls; walls is a set of 2-item sets #{a b} where a and b are locations. The returned maze is a set of the remaining walls." 
  [origin-walls]
  (let [adjacents (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                      {} (map seq origin-walls))
        start-loc (rand-nth (keys adjacents))] 
    (loop [walls origin-walls
           unvisited (disj (set (keys adjacents)) start-loc)] 
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth adjacents) loc)
              steps (zipmap (take-while unvisited walk) (next walk))]
          (recur (reduce disj walls (map set steps)) 
                 (reduce disj unvisited (keys steps))))
        walls))))

;; full walls
(defn grid [w h]
  (set (concat
        (for [i (range (dec h)) j (range w)] #{[i j] [(inc i) j]}) 
        (for [i (range h) j (range (dec w))] #{[i j] [i (inc j)]}))))

(defn draw
  [w h maze]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
     (doto (proxy [javax.swing.JPanel] []
             (paintComponent [^java.awt.Graphics g]
               (let [g (doto ^java.awt.Graphics2D (.create g)
                         (.scale 10 10)
                         (.translate 1.5 1.5)
                         (.setStroke (java.awt.BasicStroke. 0.4)))]
                 (.drawRect g -1 -1 w h)
                 (doseq [[[xa ya] [xb _]] (map sort maze)] 
                   (let [[xc yc] (if (= xa xb)
                                   [(dec xa) ya]
                                   [xa (dec ya)])] 
                     (.drawLine g xa ya xc yc))))))
       (.setPreferredSize (java.awt.Dimension.
                           (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

(defn hex-grid [w h]
  (let [vertices (set (for [y (range h) x (range (if (odd? y) 1 0) (* 2 w) 2)]
                        [x y]))
        deltas [[2 0] [1 1] [-1 1]]]
    (set (for [v vertices d deltas f [+ -]
               :let [w (vertices (map f v d))]
               :when w]
           #{v w}))))

(defn- hex-outer-walls [w h]
  (let [vertices (set (for [y (range h) x (range (if (odd? y) 1 0) (* 2 w) 2)] [x y]))
        deltas [[2 0] [1 1] [-1 1]]]
    (set (for [v vertices d deltas f [+ -]
               :let [w (map f v d)]
               :when (not (vertices w))] 
           #{v (vec w)}))))

(defn hex-draw
  [w h maze]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
     (doto (proxy [javax.swing.JPanel] []
             (paintComponent [^java.awt.Graphics g]
               (let [maze (into maze (hex-outer-walls w h))
                     g (doto ^java.awt.Graphics2D (.create g)
                         (.scale 10 10)
                         (.translate 1.5 1.5)
                         (.setStroke (java.awt.BasicStroke. 0.4
                                                            java.awt.BasicStroke/CAP_ROUND
                                                            java.awt.BasicStroke/JOIN_MITER)))
                     draw-line (fn [[[xa ya] [xb yb]]]
                                 (.draw g (java.awt.geom.Line2D$Double.
                                           xa (* 2 ya) xb (* 2 yb))))]
                 (doseq [[[xa ya] [_ yb]] (map sort maze)]
                   (draw-line (cond
                                (= ya yb) [[(inc xa) (+ ya 0.4)] [(inc xa) (- ya 0.4)]]
                                (< ya yb) [[(inc xa) (+ ya 0.4)] [xa (+ ya 0.6)]]
                                :else [[(inc xa) (- ya 0.4)] [xa (- ya 0.6)]]))))))
       (.setPreferredSize (java.awt.Dimension.
                           (* 20 (inc w)) (* 20 (+ 0.5 h))))))
    .pack
    (.setVisible true)))

(defn html-zip 
  [root] 
  (z/zipper
   vector?
   (fn [[_ & xs]]
     (if (map? (first xs)) (next xs) xs)) 
   (fn [[tagname & xs] children]
     (into (if (map? (first xs)) [tagname (first xs)] [tagname]) children))
   root))

(defn wrap
  "Wraps the current node in the specified tag and attributes." 
  ([loc tag]
   (z/edit loc #(vector tag %))) 
  ([loc tag attrs]
   (z/edit loc #(vector tag attrs %))))

(def h [:body [:h1 "Clojure"] [:p "What a wonderful language!"]])
(-> h html-zip z/down z/right z/down (wrap :b) z/root)

(def labyrinth (let [g (grid 10 10)] (reduce disj g (maze g))))
(def theseus (rand-nth (distinct (apply concat labyrinth))))
(def minotaur (rand-nth (distinct (apply concat labyrinth))))

;; Maze resolver
(defn ariadne-zip
  [labyrinth loc]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]})) 
                      {} (map seq labyrinth))
        children (fn [[from to]]
                   (seq (for [loc (paths to)
                              :when (not= loc from)] 
                          [to loc])))]
    (z/zipper (constantly true) 
              children
              nil
              [nil loc])))

(->> theseus
     (ariadne-zip labyrinth)
     (iterate z/next)
     (filter #(= minotaur (second (z/node %)))) 
     first 
     z/path
     (map second))

(defn draw-solution
  [w h maze path]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
     (doto (proxy [javax.swing.JPanel] []
             (paintComponent [^java.awt.Graphics g]
               (let [g (doto ^java.awt.Graphics2D (.create g)
                         (.scale 10 10)
                         (.translate 1.5 1.5)
                         (.setStroke (java.awt.BasicStroke. 0.4)))]
                 (.drawRect g -1 -1 w h)
                 (doseq [[[xa ya] [xb _]] (map sort maze)]
                   (let [[xc yc] (if (= xa xb) 
                                   [(dec xa) ya]
                                   [xa (dec ya)])] 
                     (.drawLine g xa ya xc yc)))
                 (.translate g -0.5 -0.5) 
                 (.setColor g java.awt.Color/RED) 
                 (doseq [[[xa ya] [xb yb]] path]
                   (.drawLine g xa ya xb yb))))) 
       (.setPreferredSize (java.awt.Dimension.
                           (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

(let [w 40
      h 40
      grid (grid w h)
      walls (maze grid)
      labyrinth (reduce disj grid walls)
      places (distinct (apply concat labyrinth)) 
      theseus (rand-nth places)
      minotaur (rand-nth places)
      path (->> theseus
                (ariadne-zip labyrinth)
                (iterate z/next)
                (filter #(= minotaur (first (z/node %)))) 
                first 
                z/path 
                rest)]
  (draw-solution w h walls path))