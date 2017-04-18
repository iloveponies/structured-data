(ns structured-data)

(defn do-a-thing [x]
  (let  [xx (+ x x)]
     (float (reduce * (repeat xx xx)))
    )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)


(defn spiff-destructuring [v]
  (let [n[ (get v 0) (get v 2)] ]
  (+ (first n) (second n)))
)


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
  )
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (- y2 y1)
  )
)

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (cond (= (- y1 y2) (- x1 x2)) true
    :else false))
)


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (let [h (height rectangle)]
  (let [w (width rectangle)]
  (* w h))))
)

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle]
   (let [[zx1 zy1] point]
   (cond
     (and (and (>= zx1 x1 ) (<= zx1 (+ x1 (width rectangle) ))  (>= zy1 y1 ) (<= zy1 (+ y1 (height rectangle) )))) true
    :else false)
     )
     )
)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (let [p1 (point x1 y1)]
    (let [p2 (point x2 y2)]
    (cond
      (and (contains-point? outer p1) (contains-point? outer p2)) true
    :else false)
    )))
)



(defn title-length [book]
  :-)

(defn author-count [book]
  :-)

(defn multiple-authors? [book]
  :-)

(defn add-author [book new-author]
  :-)

(defn alive? [author]
  :-)

(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
