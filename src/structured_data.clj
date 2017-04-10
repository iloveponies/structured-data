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
  "Exercise 15"
  (map count collection))


(defn second-elements [collection]
  "Exercise 16"
  (let [dumdum (fn [x] (get x 1))]
    (map dumdum collection)))


(defn titles [books]
  "Exercise 17"
  (map :title books))


(defn monotonic? [a-seq]
  "Exercise 19"
  (or (apply <= a-seq) (apply >= a-seq)))


(defn stars [n]
  "Exercise 18"
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  "Exercise 20"
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

;(assoc [1 2 3 4] 2 "foo") ;=> [1 2 "foo" 4]
(defn contains-duplicates? [a-seq]
  "Exercise 21"
  (> (count a-seq) (count (set a-seq))))


;(assoc {:a 1} :a 2) ;=> {:a 2}
(defn old-book->new-book [book]
  "Exercise 22"
  (assoc book :authors (set (:authors book))))


(defn has-author? [book author]
  "Exercise 23"
  (contains? (:authors book) author))


(defn authors [books]
  "Exercise 24"
  (let [authors (fn [book] (concat (:authors book)))]
    (set (apply concat (map authors books)))))


(defn all-author-names [books]
  "Exercise 25"
  (let [author-names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))


(defn author->string [author]
  "Exercise 26"
  :-)

(defn authors->string [authors]
  "Exercise 27"
  :-)

(defn book->string [book]
  "Exercise 28"
  :-)

(defn books->string [books]
  "Exercise 29"
  :-)

(defn books-by-author [author books]
  "Exercise 30"
  :-)

(defn author-by-name [name authors]
  "Exercise 31"
  :-)

(defn living-authors [authors]
  "Exercise 32"
  :-)

(defn has-a-living-author? [book]
  "Exercise 33"
  :-)

(defn books-by-living-authors [books]
  "Exercise 34"
  :-)

; %________%
