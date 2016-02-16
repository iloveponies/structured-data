(ns structured-data)

(defn do-a-thing [x]
  (let [addX (+ x x)]
    (
      Math/pow addX addX)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v (str "<3")))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (let [
        [[x1 y1] [x2 y2]] rectangle
        h1 (- y2 y1)
        w1 (- x2 x1)
        ]
    (== h1 w1)
    ))

(defn area [rectangle]
  (let [
        [[x1 y1] [x2 y2]] rectangle
        h1 (- y2 y1)
        w1 (- x2 x1)
        ]
    (* h1 w1)
    ))


  (defn contains-point? [rectangle point]
    (let [
          [[x1 y1] [x2 y2]] rectangle
          [px py] point
          ]
      (and (<= x1 px x2) (<= y1 py y2)
           )))

(defn contains-rectangle? [outer inner]
  (let [
        [[x11 y11] [x12 y12]] outer
        [[x21 y21] [x22 y22]] inner
        ]
    (and
      (and (<= x11 x21 x12) (<= y11 y21 y12))
      (and (<= x11 x22 x12) (<= y11 y22 y12))
      )))

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
