(ns structured-data)

(defn do-a-thing [x]
  (let [local_x (+ x x)]
    (Math/pow local_x local_x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v1 v2 v3] v]
    (+ v1 v3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- x2 x1)))

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2]] rectangle
         [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))


(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner ]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x))  collection))

(defn second-elements [collection]
  (let [extract-second (fn [v] (get v 1))]
    (map extract-second (seq collection))))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [original-element-count (count a-seq)
        unique-element-count (count (set a-seq))]
    (not= original-element-count unique-element-count)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors-list (map :authors books)]
    (apply clojure.set/union authors-list)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        died (:death-year author)
        born (if (contains? author :birth-year) (str " (" (:birth-year author) " - " died ")") "")]
    (str name born)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [authors-of-book (authors->string (:authors book))
        title-of-book (:title book)]
    (str title-of-book ", written by " authors-of-book)))

(defn books->string [books]
  (let [count-of-books (count books)
        count-str (cond (== 1 count-of-books) "1 book. "
                        (== 0 count-of-books) "No books"
                        :else (str count-of-books " books. "))
        books-str (map book->string books)]
        (str count-str (apply str (interpose ". " books-str)) ".")))



(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (:name a))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
