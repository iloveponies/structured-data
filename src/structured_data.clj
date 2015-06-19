(ns structured-data)

(defn do-a-thing [x]
  (let [ xx (+ x x)]
    (Math/pow xx xx)))

(do-a-thing 2)

(defn spiff [v]
  (let [[x y z & xs] v]
    (+ x z)))

(spiff [1 2 3 ])

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z & xs] v]
    (+ x z)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(def a (rectangle [1 1] [5 5]))

(defn width [rectangle]
  (let [bl (get rectangle 0)
        tr (get rectangle 1)]
    (- (get tr 0) (get bl 0))))

(width (rectangle [1 1] [5 1]))
(width (rectangle [1 1] [1 1]))
(width (rectangle [3 1] [10 4]))


(defn height [rectangle]
  (let [bl (get rectangle 0)
        tr (get rectangle 1)]
    (- (get tr 1) (get bl 1))))

(height (rectangle [1 1] [5 1]))
(height (rectangle [1 1] [5 5]))
(height (rectangle [0 0] [2 3]))

(defn square? [rectangle]
  (= (width rectangle)  (height rectangle)))

(square? (rectangle [1 1] [2 2]))
(square? (rectangle [1 1] [2 3]))

(defn area [rectangle]
    (* (width rectangle)  (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))


(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))

(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))


(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    ( and (contains-point?  outer bl) (contains-point?  outer tr))))

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2]))

(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3]))



(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))




(defn multiple-authors? [book]
  (< 1 (author-count book )))



(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))


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
