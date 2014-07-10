(ns structured-data)


;; (Math/pow (+ x x) (+ x x))

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


;; that takes a vector and returns the sum of the first and third elements

(defn spiff [v]
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  (conj v "<3"))



;; Rewrite our earlier function spiff by destructuring its parameter

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))



(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))


;; returns true if rectangle is a square and otherwise false

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))



;; returns the area of the given rectangle.

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))



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
