(ns structured-data)

(defn do-a-thing [x]
  (let [x1 (+ x x)]
  (Math/pow x1 x1)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z][(get v 0) (get v 1) (get v 2)]]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (== (- y2 y1) (- x2 x1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (* (- y2 y1) (- x2 x1))))

(defn contains-point? [rectangle point]
  (let [[x3 y3] point]
    (let [[[x1 y1] [x2 y2]] rectangle]
          (and (<= x1 x3 x2) (<= y1 y3 y2)) )))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (get inner 1))
       (contains-point? outer (get inner 0))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let [original ]))

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
