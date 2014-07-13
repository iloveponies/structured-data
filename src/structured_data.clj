(ns structured-data)

(defn do-a-thing [x]
  (let [x-plus-x (+ x x)]
    (Math/pow x-plus-x x-plus-x)))

(defn spiff [v]
  (+
   (get v 0)
   (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

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
  (let [h (height rectangle) w (width rectangle)]
    (== h w)))

(defn area [rectangle]
  (let [h (height rectangle) w (width rectangle)]
    (* h w)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1] [rx2 ry2]] rectangle
        [px py] point]
    (and
     (<= rx1 px rx2)
     (<= ry1 py ry2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bot-left inner-top-right] inner]
    (and
     (contains-point? outer inner-bot-left)
     (contains-point? outer inner-top-right))))

(defn title-length [book]
  (let [title (:title book)]
    (count title)))

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
