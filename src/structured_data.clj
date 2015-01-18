(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let[[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 ]
  (- x2 x1)))

(defn height [rectangle]
  (let [ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 ]
  (- y2 y1)))

(defn square? [rectangle]
  (if(== (width rectangle) (height rectangle))
    true
    false))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 [px py] point]
    (and
     (<= x1 px x2)
     (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and
      (contains-point? outer p1)
      (contains-point? outer p2))))

(defn title-length [book] (count (:title book)))

(defn author-count [book] (count (:authors book)))

(defn multiple-authors? [book] (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors-new (conj (:authors book) new-author)]
    (assoc book :authors authors-new)))

(defn alive? [author]
  (not (contains? author :death-year)))

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
