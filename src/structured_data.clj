(ns structured-data)

; exercise-1
(defn do-a-thing [x]
  (let [y (+ x x)]
  (Math/pow y y)))

; exercise-2
(defn spiff [v]
  (+ (get v 0) (get v 2)))

; exercise-3
(defn cutify [v]
  (conj v "<3"))

; exercise-4
(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

; exercise-5
(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- x1 x0)))

(defn height [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- y1 y0)))

; exercise-6
(defn square? [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (== (- x0 x1) (- y0 y1))))

; exercise-7
(defn area [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
      (* (- x0 x1) (- y0 y1))))

; exercise-8
(defn contains-point? [rectangle point]
  (let [[[x0 y0] [x1 y1]] rectangle
        [x y] point]
    (and (<= x0 x x1) 
         (<= y0 y y1))))

; exercise-9
(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
       (and (contains-point? outer bottom-left) 
            (contains-point? outer top-right))))

; exercise-10
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
