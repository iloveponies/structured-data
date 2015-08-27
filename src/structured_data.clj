(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)] (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))  

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
      (and (<= x1 x3 x2) (<= y1 y3 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and
      (contains-point? outer [x1 y1])
      (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [original-authors (:authors book)]
    (assoc book :authors (conj original-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [x] (get x 1))]
    (map helper collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [original-authors (:authors book)]
    (assoc book :authors (set original-authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors (fn [book] (:authors book))]
    (apply clojure.set/union (map authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)]
    (let [years (str " (" (:birth-year author) " - " (:death-year author) ")")]
      (str author-name (if (:birth-year author) years)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-title (:title book)]
    (let [authors (authors->string (:authors book))]
      (str book-title (if authors (str ", written by " authors))))))

(defn books->string [books]
  (let [book-count (count books)]
    (let [book-count-string 
      (cond
        (= book-count 0) "No books"
        (= book-count 1) "1 book. "
        :else (str book-count " books. "))]
      (let [books-string
        (apply str (interpose ". " (map book->string books)))]
        (str book-count-string books-string ".")))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
