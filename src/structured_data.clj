(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[p1 p2]]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (- x2 x1)))

(defn height [[p1 p2]]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (- y2 y1)))

(defn square? [[p1 p2]]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (= (- x2 x1) (- y2 y1))))

(defn area [[p1 p2]]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [[p1 p2] point]
  (let [[x1 y1] p1
        [x2 y2] p2
        [x y] point]
    (and (>= x1 x x2) (>= y1 y y2))))

(defn contains-rectangle? [outer inner]
  :-)

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
