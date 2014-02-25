(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


(defn spiff [v]
 (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v] (+ a c)))

(defn point [x y] [x y])

(defn rectangle [bottom-left top-right]  [bottom-left top-right])

(defn width [rectangle]
 (let [[[x1 y1][x2 y2]] rectangle]
   (- x2 x1)))

(defn height [rectangle]
   (let [[[x1 y1][x2 y2]] rectangle ]
     (- y2 y1)))

(defn square? [rectangle]
  (= 0 (- (width rectangle) (height rectangle))))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [xx1 yy1] point]
    (and (<= x1 xx1 x2) (<= y1 yy1 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] outer
        [[xx1 yy1][xx2 yy2]] inner]
     (and (<= x1 xx1 xx2 x2) (<= y1 yy1 yy2 y2))
 ))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count(get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))))

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
