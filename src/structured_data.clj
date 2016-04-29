(ns structured-data)

(defn do-a-thing [x]
  (let [doubble (+ x x)]
    (Math/pow doubble doubble)))

(defn spiff [v]
  (let [vec v]
    (if (>= (count vec) 3)
    (+ (get vec 0) (get vec 2)) nil)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (if (not (= c nil)) (+ a c) nil)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (- (first (second rectangle)) (first (first rectangle))))

(defn height [rectangle]
  (- (second (second rectangle)) (second (first rectangle))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (and (<= (first (first rectangle)) (first point) (first (second rectangle)))
       (<= (second (first rectangle)) (second point) (second (second rectangle)))))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (first inner))
       (contains-point? outer (second inner))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (book :authors)]
  (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
  (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)
  (count (set a-seq)))))

(defn old-book->new-book [book]
(assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (str (:name author)
    (if (contains? author :birth-year)
    (str " (" (:birth-year author) " - "
    (:death-year author) ")"))))

(defn authors->string [authors]
  (apply str (interpose ", "
  (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by "
  (authors->string (:authors book))))

(defn books->string [books]
    (if (empty? books) "No books."
      (str (count books) " book"
      (if (< 1 (count books)) "s") ". "
      (apply str (interpose ". "
      (map book->string books)))".")))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name))
    authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
