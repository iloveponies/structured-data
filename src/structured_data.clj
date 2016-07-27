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
  (let [[[x1 y1] [x2 y2]] rectangle]
     (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
     (- y2 y1)))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle)) true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[px py] point [[rx1 ry1] [rx2 ry2]] rectangle]
    (if (and (>= ry2 py ry1) (>= rx2 px rx1)) true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (if (and (contains-point? outer point1) (contains-point? outer point2)) true
      false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true
    false))

(defn add-author [book new-author]
  (let [new-authors (conj (book :authors) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (if (contains? author :death-year) false
    true))

(defn element-lengths [collection]
  (let [elem-length (fn [x] (count x))]
    (map elem-length collection)))

(defn second-elements [collection]
  (let [second-elem (fn [x] (get x 1))]
    (map second-elem collection)))

(defn titles [books]
  (let [title (fn [x] (get x :title))]
    (map title books)))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true
    false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (if (contains? (book :authors) author) true
    false))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (let [author-name (fn [x] (x :name))]
    (set (map author-name (authors books)))))

(defn author->string [author]
  (let [author-name (fn [x] (x :name))
    author-years (fn [x] (str " (" (x :birth-year) " - " (x :death-year) ")"))]
      (str (author-name author) (if (contains? author :birth-year) (author-years author)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-title (fn [x] (x :title))
    book-author (fn [x] (authors->string (x :authors)))]
      (str (book-title book) ", written by " (book-author book))))

(defn books->string [books]
  (let [book-count (fn [x] (str (count x) (if (> (count x) 1) " books. " " book. ")))]
    (if (= (count books) 0) "No books."
      (str (book-count books) (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (get x :name))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (get book :authors))) false
    true))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
