(ns structured-data)

(defn do-a-thing [x]
  (Math/pow (+ x x) (+ x x)))

(defn spiff [v]
  (if (and (= (get v 0) nil) (= (get v 2) nil))
    "?"
    (+ (get v 2) (get v 0))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (and (= (get v 0) nil) (= (get v 2) nil))
    "?"
    (let [[a b c] v]
    (+ a c))))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> x1 x2)
      (- x1 x2)
      (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2)
      (- y1 y2)
      (- y2 y1))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2)
      (if (> x1 x2)
      (= (- x1 x2) (- y1 y2))
      (= (- x2 x1) ((- y2 y1))))
      (if (> x1 x2)
      (= (- x1 x2) (- y1 y2))
      (= (- x2 x1) (- y2 y1))))))


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2)
      (if (> x1 x2)
      (* (- x1 x2) (- y1 y2))
      (* (- x2 x1) ((- y2 y1))))
      (if (> x1 x2)
      (* (- x1 x2) (- y1 y2))
      (* (- x2 x1) (- y2 y1))))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
      (and (>= x1 x3) (<= x2 x3) (>= y1 y3) (<= y2 y3)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[x3 y3] [x4 y4]] inner]
      (and (contains-point? (rectangle [x1 y1] [x2 y2])
                 (point x3 y3)) (contains-point? (rectangle [x1 y1] [x2 y2])
                 (point x4 y4))))))


(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
(count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (count (get book :authors))))

(defn add-author [book new-author]
  (let [original book
      new      (assoc original :authors new-author)]
  (conj (assoc original :title nil) new-author)))



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

