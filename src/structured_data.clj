(ns structured-data)

(defn do-a-thing [x]
  (let [xx (* x x)]
    (Math/pow xx xx)
   )
  )

(do-a-thing 2)
()

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(spiff [1 2 3])
(defn cutify [v]
  (conj v "<3")
  )
(cutify [1])

(defn spiff-destructuring [v]
  (let [[x z y]v]
    (+ x y)
  )
  )
(spiff-destructuring [1 2 3])

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )
9
(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )
(height (rectangle [1 1] [5 1])) ; 0
(height (rectangle [1 1] [5 5])) ; 4
(height (rectangle [0 0] [2 3])) ; 3

(width (rectangle [1 1] [5 1]))  ; 4
(width (rectangle [1 1] [1 1]))  ; 0
(width (rectangle [3 1] [10 4])) ; 7

(defn square? [rec]
  (if (= (height rec) (width rec)) true false)
  )
(square? (rectangle [1 1] [2 2])) ;=> true
(square? (rectangle [1 1] [2 3])) ;=> false
(square? (rectangle [1 1] [1 1])) ;=> true
(square? (rectangle [3 2] [1 0])) ;=> true
(square? (rectangle [3 2] [1 1])) ;=> false

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
