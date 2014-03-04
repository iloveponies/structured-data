(ns structured-data)

(defn do-a-thing [x]
  (let [dbl (+ x x)]
    (Math/pow dbl dbl)))

(defn spiff [v]
  (if (<= 3 (count v))
    (+ (first v) (nth v 2))
    '?))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (<= 3 (count v))
    (let [[fst _ trd] v]
      (+ fst trd))
    '?))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
 (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
    (let [[[x1 y1][x2 y2]] rectangle
          [x y] point]
      (and (<= (min x1 x2) x (max x1 x2)) (<= (min y1 y2) y (max y1 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and (contains-point? outer inner-bottom-left) (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  ((complement contains?) author :death-year))

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
