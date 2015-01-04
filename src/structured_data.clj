(ns structured-data)

(defn do-a-thing [x] (let [sum (+ x x)] (Math/pow sum sum)))


(defn spiff [x] (+ (get x 2) (get x 0)))

(defn cutify [x] (conj x "<3"))


(defn spiff-destructuring [[x1 x2 x3]] (+ x1 x3))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [[[x1 y1] [x2 y2]]] (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]] (- y2 y1))

(defn abs [x] (if (< x 0) (* x -1) x))

(defn square? [[rec1 rec2]] (let [[x1 y1] rec1 [x2 y2] rec2] (= (abs(- x1 x2)) (abs(- y2 y1)))))


(defn area [[rec1 rec2]] (let [[x1 y1] rec1 [x2 y2] rec2] (* (abs(- y2 y1)) (abs(- x2 x1)))))


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
