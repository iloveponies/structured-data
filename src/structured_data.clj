(ns structured-data)

(defn do-a-thing [x]
  (let [sx (+ x x)]
    (Math/pow sx sx)))

(defn spiff [v]
  (let [v1 (get v 0)
        v3 (get v 2)]
    (+ v1 v3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v1 x v3] v]
    (+ v1 v3)))

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
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

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
  (let [authors (:authors book)
        new (conj authors new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [elements] (get elements 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (let [author
        (fn [book] (:authors book))]
    (apply clojure.set/union (map author books))))

(defn all-author-names [books]
  (let [authors (authors books)]
    (set (map :name authors))))

(defn author->string [author]
  (let [name (:name author)
        years (if (contains? author :birth-year)
                (str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors-string (authors->string (:authors book))]
    (str title ", written by " authors-string)))

(defn books->string [books]
  (let [total (count books)
        books-string (apply str (interpose ". " (map book->string books)))]
    (cond
      (= total 0) "No books."
      (= total 1) (str "1 book. " books-string ".")
      :else       (str total " books. " books-string "."))))


(defn books-by-author [author books]
  (let [filter-author (fn [book] (has-author? book author))]
    (filter filter-author books)))

(defn author-by-name [name authors]
  (let [filter-name (fn [author] (= name (:name author)))]
    (first (filter filter-name authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
