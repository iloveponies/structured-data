(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn abs [n]
  (if (< n 0) (- n) n))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (- y2 y1))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= (min x1 x2) x (max x1 x2))
         (<= (min y1 y2) y (max y1 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book []) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  ((if (contains? a-set elem) disj conj) a-set elem))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (for [book books author (:authors book)] author)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth-year (:birth-year author)]
    (if birth-year
      (format "%s (%s - %s)" (:name author) birth-year (:death-year author ""))
      (:name author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (format "%s, written by %s" (:title book) (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (empty? books) "No books."
   (= 1 (count books)) (format "1 book. %s." (book->string (first books)))
   :else (format "%d books. %s." (count books) (apply str (interpose ", " (map book->string books))))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
