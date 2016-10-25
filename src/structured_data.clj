(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x )]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1][x2 y2]]]
    (- x2 x1))

(defn height [[[x1 y1][x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1][rx2 ry2]] rectangle]
    (let [[px py] point]
      (and (<= rx1 px rx2) (<= ry1 py ry2)))))

(defn contains-rectangle? [outer [inner-first inner-second]]
  (and (contains-point? outer inner-first) (contains-point? outer inner-second)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (not (== 1 (author-count book))))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-length [element]
  (count element))

(defn element-lengths [collection]
  (map element-length collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

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
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
 (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [author-names (set (map :name (:authors book)))]
    (contains? author-names (:name author))))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->life-string [author]
  (if (contains? author :birth-year)
    (str " (" (:birth-year author) " - " (:death-year author) ")")
    ))

(defn author->string [author]
  (let [name (:name author)]
    (str name (author->life-string author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [author (:authors book)]
    (let [title (:title book)]
      (str title ", written by " (authors->string author)))))

(defn book-count->string [book-count]
  (if (== 0 book-count)
    "No books."
    (if (== 1 book-count)
      "1 book."
      (str book-count " books."))))

(defn books->title-author-string [books]
  (apply str (interpose ". " (map book->string books))))

(defn books->string [books]
  (let [book-count (count books)]
    (if (== 0 book-count)
      "No books."
      (let [book-count-text (book-count->string book-count)]
        (let [books-text (books->title-author-string books)]
          (str book-count-text " " books-text "."))))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [filtered-authors (filter (fn [author] (= name (:name author))) authors)]
    (if (== 0 (count filtered-authors))
      nil
      (first filtered-authors))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
