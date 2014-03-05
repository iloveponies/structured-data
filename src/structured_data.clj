(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [elem1 (get v 0)
        elem3 (get v 2)]
    (+ elem1 elem3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[elem1 _ elem3] v]
    (+ elem1 elem3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1] [rx2 ry2]] rectangle
        [px py] point]
    (and (<= rx1 px rx2) (<= ry1 py ry2))))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
    (and (<= ix2 ox2) (<= iy2 oy2) (<= ox1 ix1) (<= oy1 iy1))))

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
  (map count collection))

(defn second-elements [collection]
  (let [get-second-elem (fn [x] (get x 1))]
    (map get-second-elem collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [orig-count (count a-seq)]
    (< (count (set a-seq)) orig-count)))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [auth-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
      (contains? author :death-year) (str auth-name " (" birth-year " - " death-year ")")
      (contains? author :birth-year) (str auth-name " (" birth-year " - " ")")
      :else auth-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by "(authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)]
    (cond
      (= 1 book-count) (str book-count " book. " (apply str (interpose ". " (map book->string books))) ".")
      (< 1 book-count) (str book-count " books. " (apply str (interpose ". " (map book->string books))) ".")
      :else "No books.")))

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
