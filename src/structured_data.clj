(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[c0 c1 c2 c3] v]
    (+ c0 c2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x3 y3]] rectangle
         [x2 y2]         point]
    (and (<= x1 x2 x3)
         (<= y1 y2 y3))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer top-right)
         (contains-point? outer bottom-left))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [sub-collection] (get sub-collection 1))]
    (map second collection)))

(defn titles [books]
  (let [title (fn [book] (:title book))]
    (map title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors (fn [book] (map :authors book))]
      (apply clojure.set/union (authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name       (str (:name author))
        years      (cond (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
                         (contains? author :birth-year) (str " (" (:birth-year author) " - )")
                         :else "")]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [count-books     (count books)
        count-books-str (cond (== count-books 0) "No books"
                              (== count-books 1) "1 book. "
                              :else (str count-books " books. "))]
    (str count-books-str (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
