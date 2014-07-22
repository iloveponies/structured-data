(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0)(get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (spiff [x y z])))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
  (- x1 x0)))

(defn height [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
  (- y1 y0)))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x0 y0] [x1 y1]] rectangle]
  (let [[x y] point]
  (if (and (<= x0 x x1) (<= y0 y y1))
    true
    false))))

(defn contains-rectangle? [outer inner]
  (let [[[ix0 iy0] [ix1 iy1]] inner]
  (if (and (contains-point? outer [ix0 iy0])
           (contains-point? outer [ix1 iy1]))
    true
    false)))

(defn title-length [book]
  (count(:title book)))

(defn author-count [book]
  (count(:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
  (assoc book :authors authors)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [collection] (get collection 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [settified (set a-seq)]
    (if (== (count settified) (count a-seq))
      false
      true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (:birth-year author)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (:name author)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
(let [booklist (apply str (interpose ", " (map book->string books)))]
  (str (cond
         (= (count books) 0) (str "No books.")
         (= (count books) 1) (str "1 book. " booklist ".")
         :else (str (count books) " books. " booklist ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
    (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
