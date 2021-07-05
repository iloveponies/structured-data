(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (let [[[x1 y2] [x2 y1]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y2] [x2 y1]] rectangle]
    (- y1 y2)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (+ x1 y2) (+ x2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [px py] point]
    (and (and (>= px x1) (>= py y1)) (and (<= px x2) (<= py y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
    (and (and (>= ix1 x1) (>= iy1 y1)) (and (<= ix2 x2) (<= iy2 y2)))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [kk (fn [x] (get x 1))]
    (map kk collection)))

(defn titles [books]
  (let [mm (fn [c] (:title c))]
    (map mm books)))

(defn monotonic? [a-seq]
  (or (or (apply >= a-seq)) (or (apply <= a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-names (fn [x] (:authors x))]
  (set (apply clojure.set/union (map author-names books)))))

(defn all-author-names [books]
  (let [author-names (fn [x] (map :name (:authors x)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [a-name (fn [x] (:name x))
        a-years (fn [x] (if (contains? x :birth-year) (str " (" (:birth-year x) " - " (:death-year x) ")") (str "")))]
  (str (a-name author) (a-years author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-amount (count books)]
  (if (> book-amount 0) (if (> book-amount 1) (str book-amount " books. " (apply str (interpose ". " (map book->string books))) ".") (str book-amount " book. " (apply str (map book->string books)) ".")) (str "No books."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [name-test (fn [x] (if (= (:name x) name) true false))]
    (if (empty? (filter name-test authors)) nil (first (filter name-test authors)))))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (filter (fn [x] (alive? x)) (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
