(ns a-star.core)

(defrecord Location [x y])

(defrecord Node [location parent h])

(defn x [node]
  (:x (:location node)))

(defn y [node]
  (:y (:location node)))

(defn get-children [parent, h]
  (let [parent-x (x parent)
        parent-y (y parent)
        new-locations [(Location. (inc parent-x) parent-y)
                       (Location. (dec parent-x) parent-y)
                       (Location. parent-x (inc parent-y))
                       (Location. parent-x (dec parent-y))]]
    (map #(Node. % parent (h %)) new-locations)))

(defn distance [end node]
  (let [dx (- (:x node) (:x end))
        dy (- (:y node) (:y end))]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn find-path [node]
  (reverse
   (for [node (iterate :parent node)
         :while node]
     {:x (x node) :y (y node)})))

(defn compy [a b]
  (compare ((juxt :h x y) a) ((juxt :h x y) b)))

(defn distance-from-end [end]
  (partial distance end))

(defn a-star [start end]
  (let [h (distance-from-end end) start-node (Node. start nil (h start))]
    (loop [open (sorted-set-by compy start-node)
           closed #{}]
      (if (seq open)
        (let [current (first open)]
          (cond
            (= (:location current) end)
            (find-path current)
            (contains? closed (:location current))
            (recur (disj open current) closed)
            :else (recur (into (disj open current) (get-children current h)) (conj closed (:location current)))))
        "No path found"))))

(defn -main []
  (let [start (Location. 0 0)
        end (Location. 100 100)]
    (time (a-star start end))))
