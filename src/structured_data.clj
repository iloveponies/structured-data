(ns structured-data)

(defn do-a-thing [x]
  (let [plussaa (+ x x)]
    (Math/pow plussaa plussaa)))




(defn spiff [v]
  (+ (get v 0) (get v 2)))





(defn cutify [v]
  (conj v "<3"))




(defn spiff-destructuring [v]
  (let [[x y z] v]
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
  (if (== (height rectangle) (width rectangle)) true false))




(defn area [rectangle]
  (* (height rectangle) (width rectangle)))



(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a b] point]
    (if (and (<= x1 a x2) (<= y1 b y2)) true false)))





(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2))) true false)))





(defn title-length [book]
  (count (:title book)))




(defn author-count [book]
  (count (:authors book)))



(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))




(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))





(defn alive? [author]
  (if (contains? author :death-year) false true))




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
