(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)
    ))

(defn spiff [v]
  (if (and (vector? v) (>= (count v) 3))
    (+ (get v 0) (get v 2))
   ))

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (if (and (vector? v) (>= (count v) 3))
    (let [[x y z] v]
      (+ x z)
      )
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))
    ))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[[x1 y1][x2 y2]] [px py]] [rectangle point]]
    (and (<= x1 px x2) (<= y1 py y2))))  

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-corner inner-top-corner] inner]
    (and (contains-point? outer inner-bottom-corner)
         (contains-point? outer inner-top-corner))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

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
