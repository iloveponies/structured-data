(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [x (get v 0) y (get v 2)]
  (+ x y))
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x y] [x1 y1]] rectangle]
  (- x1 x)))

(defn height [rectangle]
  (let [[[x y] [x1 y1]] rectangle]
  (- y1 y)))

(defn square? [rectangle]
  (let [[[x y] [x1 y1]] rectangle]
  (= (- x1 x) (- y1 y) )))

(defn area [rectangle]
  (let [[[x y] [x1 y1]] rectangle]
  (* (- x1 x) (- y1 y) )))

(defn contains-point? [rectangle point]
  (let [[[x2 y2] [x1 y1]] rectangle [x y] point]
  (and (<= x2 x x1) (<= y2 y y1))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[x3 y3] [x4 y4]] inner]
  (and (<= x1 x3 x2) (<= y1 y3 y2) (<= x1 x4 x2) (<= y1 y4 y2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (count (:authors book))) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (set (:authors book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year) (str (:name author)" ("(:birth-year author)" - "(:death-year author)")") (str (:name author)) ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (empty? books) "No books."
   (= (count books) 1) (str "1 book. " (book->string (get books 0))".")
   (> (count books) 1) (str (count books)" books. " (apply str (interpose ". " (map book->string books)))".")
   ))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (get x :name))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
