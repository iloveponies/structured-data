(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
  (Math/pow x+x x+x)))

(defn spiff [v]
  "Returns the sum of the first and third elements of a vector"
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  "Adds <3 to the end of a vector"
  (conj v "<3"))

(defn spiff-destructuring [v]
  "Returns the sum of the first and third elements of a vector-DESTRUCTURE STYLE!"
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
  (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  "Returns true if rectangle contains point and otherwise false"
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
  (if (and (<= x1 px x2) (<= y1 py y2)) true false)))

(defn contains-rectangle? [outer inner]
  "Returns true if the rectangle inner is inside the rectangle outer and otherwise false"
  (let [[ix iy] inner]
    (if (and (contains-point? outer ix)
           (contains-point? outer iy)) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (>= (author-count book) 2) true false))

(defn add-author [book new-author]
  "Takes a book and an author and adds author to books authors"
  (let [authors (get book :authors)
        updated-authors (conj authors new-author)]
    (assoc book :authors updated-authors)))

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
