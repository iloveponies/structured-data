(ns structured-data)

(defn do-a-thing [x]
  "Exercise 1"
  (let [xx (+ x x)]
    (Math/pow xx xx)))


(defn spiff [v]
  "Exercise 2"
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  "Exercise 3"
  (conj v "<3"))


(defn spiff-destructuring [v]
  "Exercise 4"
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [rectangle]
  "Exercise 5"
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  "Exercise 5"
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))


(defn square? [rectangle]
  "Exercise 6"
  (let [[[x1 y1] [x2 y2]] rectangle]
    (and (== x1 y1) (== x2 y2) true)))


(defn area [rectangle]
  "Exercise 7"
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  "Exercise 8"
  (let [[x y] point]                                        ;destructure point
    (let [[[x1 y1] [x2 y2]] rectangle]                      ;destructure rectangle
      (and (<= x1 x x2) (<= y1 y y2) true))))


(defn contains-rectangle? [outer inner]
  "Exercise 9"
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[ix1 iy1] [ix2 iy2]] inner]
      (and (contains-point? outer [ix1 iy1]) (contains-point? outer [ix2 iy2]) true))))


(defn title-length [book]
  "Exercise 10"
  (count (:title book)))


(defn author-count [book]
  "Exercise 11"
  (count (:authors book)))


(defn multiple-authors? [book]
  "Exercise 12"
  (> (count (:authors book)) 1))

; (assoc a-map a-key a-value) sets the value of a-key in a-map to be a-value
(defn add-author [book new-author]
  "Exercise 13"
  (let [orig_authors (:authors book)]
    (let [orig_title (:title book)]
      (assoc (if orig_title {:title orig_title}) :authors (conj (:authors book) new-author)))))


;(contains? a-map a-key) can be used to check if a-map has a value for a-key
(defn alive? [author]
  "Exercise 14"
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
