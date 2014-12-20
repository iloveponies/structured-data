(ns structured-data)

(defn do-a-thing [x]
  (let [doubleX (+ x x)]
  (Math/pow doubleX doubleX)))

(defn spiff [v]
  (cond
   (> (count v) 2) (+ (get v 0) (get v 2))
   :else "?"))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (cond
   (> (count v) 2)
     (let [[a b c] v ] (+ a c))
   :else "?"))

(defn point [x y]
  [x y])

(defn abs [x]
  (cond
   (>= x 0) x
   :else (- x)))

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
  (abs (- y1 y2))))

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
  (abs (- x1 x2))))

(defn square? [rectangle]
  (= (width rectangle)(height rectangle)))

(defn area [rectangle]
  (* (height rectangle)(width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle]
    (let [[pointX pointY] point]
      (and (<= x1 pointX x2) (<= y1 pointY y2)))))

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
