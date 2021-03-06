(ns org.dcousineau.clojure.eight-puzzle
    (:use [clojure.contrib.def :only (defvar)])
    (:use [clojure.contrib.seq :only (positions)])
    (:use [clojure.contrib.math :only (abs floor)]))

(defvar *operations* '(:up :down :left :right)
    "List of available operations")

(defvar *goal-state* '(1 2 3 8 0 4 7 6 5)
    "The desired goal state of any given board")

; Goal-state board translates to the following physical layout:
;
; 1 2 3
; 8 0 4
; 7 6 5

(defn locate [piece board]
    "Locate a piece on the board"
    (first (positions #{piece} board)))

(defn swap-pieces [loc1 loc2 board]
    "Swap 2 pieces on the board by their index"
    (let [minloc (min loc1 loc2)
          maxloc (max loc1 loc2)]
    (let [[pre-min 
           post-min] (split-at minloc board)]
    (let [[post-min-pre-max
           post-min-post-max] (split-at (- maxloc (count pre-min) 1) (rest post-min))]
        (concat 
            pre-min
            (list (nth board maxloc))
            post-min-pre-max
            (list (nth board minloc))
            (rest post-min-post-max))))))

(defn op-valid? [op board]
    "Validate an operation on the empty piece"
    (let [pos (locate 0 board)]
        (cond 
            (= op :up) (> pos 2)
            (= op :down) (< pos 6)
            (= op :left) (not= 0 (mod pos 3))
            (= op :right) (not= 2 (mod pos 3)))))

(defn apply-op [op board]
    {:pre [(op-valid? op board)]
     :doc "Move the empty piece in a given direction"}
    (let [pos (locate 0 board)]
        (cond 
            (= op :up) (swap-pieces pos (- pos 3) board)
            (= op :down) (swap-pieces pos (+ pos 3) board)
            (= op :left) (swap-pieces pos (- pos 1) board)
            (= op :right) (swap-pieces pos (+ pos 1) board))))

(defn not-backwards? [op history]
    "Is a given operation the reverse of the previous operation"
    (if (> 0 (count history))
        (let [last-op (first history)]
            (cond 
                (= last-op :up) (not= op :down)
                (= last-op :down) (not= op :up)
                (= last-op :right) (not= op :left)
                (= last-op :left) (not= op :right)))
        true))

(defn expand [history-node]
    "Produces a set of moves from a given node"
    (let [board (:board history-node)]
        (remove nil? (map 
            (fn [op]
                (if (and (op-valid? op board) (not-backwards? op (:oplist history-node)))
                    {:board (apply-op op board) 
                     :oplist (cons op (:oplist history-node)) 
                     :heuristic (composite-heuristic board)})) 
            *operations*))))

; --- A* ---

(defn manhattan-distance [piece board]
    "Find the mahattan distance between a piece and its goal state"
    (let [pos (locate piece board) 
          goal-pos (locate piece *goal-state*)]
        (abs (+ 
            (- (floor (/ pos 3)) (floor (/ goal-pos 3))) 
            (- (mod pos 3) (mod goal-pos 3))))))

(defn manhattan-distance-sum [board]
    "Find the sum of the manhattan distances for each piece on a board between their respective goal states"
    (reduce + (map #(manhattan-distance % board) board)))

(defn out-of-place [board]
    "Find the number of pieces out of place on a board compared to its goal state"
    (reduce + (map #(if (not= (locate % board) (locate % *goal-state*)) 1 0) board)))

(defn composite-heuristic [board]
    (+ (manhattan-distance-sum board) (out-of-place board)))

(defn a-star
    "Execute an A* algorithm and return an ordered list of operations to solve the board"
    ([init] 
        (a-star (list {:board init :oplist '() :heuristic (composite-heuristic board)}) 1))
    ([queue iter]
        (if (= (:board (first queue)) *goal-state*)
            (reverse (:oplist (first queue)))
            (recur (sort-by :heuristic (concat (expand (first queue)) (rest queue))) (inc iter)))))

(defvar board '(0 2 3 1 8 4 7 6 5)
    "Starting board (solution :down :right)")

(a-star board)