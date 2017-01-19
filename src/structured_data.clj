(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
)

(defn spiff [v]
  (cond
    (empty? v) 0
    (> 3 (count v)) (get v 0)
    :else (+ (get v 0) (get v 2))
  )
)


(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[a b c d] v]
    (cond
      (= a nil) 0
      (= c nil) a
      :else (+ a c)
    )
  )
)


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> x1 x2) (- x1 x2) (- x2 x1)))
)


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2) (- y1 y2) (- y2 y1)))
)

(defn square? [rectangle]
  (== (width rectangle) (height rectangle))
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle
          [px py] point]
      (and (>= px x1) (<= px x2)
           (>= py y1) (<= py y2))
      )
)

(defn contains-rectangle? [outer inner]
    (let [[[x1 y1] [x2 y2]] outer
          [[px1 py1] [px2 py2]] inner]
      (and (contains-point? outer [px1 py1])
           (contains-point? outer [px2 py2])
      )
  )
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
