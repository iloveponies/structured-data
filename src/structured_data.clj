(ns structured-data)

(defn do-a-thing [x]
  (let [z (+ x x)]
    (Math/pow z z)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x, y, z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
	(if (> x2 x1)
      (- x2 x1)
      (- x1 x2)))

(defn height [[[x1 y1] [x2 y2]]]
  	(if (> y2 y1)
      (- y2 y1)
      (- y1 y2)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]][x y]]
  (if (and (<= x1 x x2) (<= y1 y y2))
    true
    false))

(defn contains-rectangle? [outer [point1 point2]]
  (and (contains-point? outer point1) (contains-point? outer point2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  :-)

(defn multiple-authors? [book]
  (let [count (author-count book)]
    (cond
     (= count 0) false
     (= count 1) false
     :else true
     )))

(defn add-author [book new-author]
  (conj (:authors book) new-author)) ;TODO

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