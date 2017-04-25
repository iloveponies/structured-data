(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
    (+ x z))

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
    (let [[x y] point]
      (and (<= x x2) (<= y y2) (>= x x1) (>= y y1)))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) (contains-point? outer point2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [se (fn [v] (get v 1))]
    (map se collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (str (apply str (repeat n "*"))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc (dissoc book :authors) :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        byear (:birth-year author)
        dyear (:death-year author)]
          (if byear
            (if dyear
              (str name " (" byear " - " dyear ")")
              (str name " (" byear " - )"))
            name)))

(defn authors->string [authors]
  (if (empty? authors)
    ""
    (if (= (count authors) 1)
      (author->string (first authors))
      (interpose ", " (map author->string authors)))))

(defn book->string [book]
 (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (if (= (count books) 1)
      (str "1 book. " (book->string (first books)) ".")
      (str (count books) " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (if (empty? (filter (fn [author] (= name (:name author))) authors))
    nil
    (first (filter (fn [author] (= name (:name author))) authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author?   books))

; %________%
