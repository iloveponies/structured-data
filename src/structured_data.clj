(ns structured-data)

(defn
  do-a-thing
  "First, doubles the input value. Then, returns the power of the result value by itself."
  [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff
  "Returns the sum of the first and third index of the input vector."
  [v]
  (let [xx (get v 0)
        yy (get v 2)]
    (+ xx yy)))

(defn cutify
  "Adds a '<3' to the end of the input vector."
  [v]
  (let [xx "<3"]
    (conj v xx)))

(defn spiff-destructuring [v]
  "Returns the sum of the first and third index of the input vector."
  [v]
  (let [[x _ y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  :-)

(defn height [rectangle]
  :-)

(defn square? [rectangle]
  :-)

(defn area [rectangle]
  :-)

(defn contains-point? [rectangle point]
  :-)

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
