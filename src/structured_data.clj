(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[v1 v2 v3]]
  (+ v1 v3))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 x2] [y1 y2]]]
  (- y1 x1))

(defn height [[[x1 x2] [y1 y2]]]
  (- y2 x2))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 x2] [y1 y2]] [z1 z2]]
  (if (and (<= x1 z1 y1) (<= x2 z2 y2))
    true
    false))

(defn contains-rectangle? [outer [x y]]
  (if (and (contains-point? outer x) (contains-point? outer y))
    true
    false))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (let [h (fn [book] (set (:authors book)))]
    (apply clojure.set/union (map h books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond
    (not (alive? author)) (apply str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (contains? author :birth-year) (apply str (:name author) " (" (:birth-year author) " - )")
    :else (str (:name author) )))

(defn authors->string [authors]
  (apply str (interpose ", " (set(map author->string authors)))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (< (count books) 1) "No books."
    (= (count books) 1) (apply str "1 book. " (book->string (first books)) ".")
    :else (apply str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
