(ns a-star.core)

(defrecord Location [x y])

(defrecord Node [location parent h])

(defn x [node]
  (:x (:location node)))

(defn y [node]
  (:y (:location node)))

(defn get-children [parent, h]
  (let [parent-x (x parent)
        parent-y (y parent)]
    [(Location. (inc parent-x) parent-y)
     (Location. (dec parent-x) parent-y)
     (Location. parent-x (inc parent-y))
     (Location. parent-x (dec parent-y))]))

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
          (if (= (:location current) end)
            (find-path current)
            (let [next-locs (remove closed (get-children current h))]
              (recur (into (disj open current) (map #(->Node % current (h %)) next-locs))
                     (conj closed (:location current))))))
        "No path found"))))

(defn -main []
  (let [start (Location. 0 0)
        end (Location. 100 100)]
    (time (a-star start end))))
