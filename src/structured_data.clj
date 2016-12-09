(ns structured-data)

(defn do-a-thing [x]
  (let [dbl (+ x x)]
    (Math/pow dbl dbl)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (and
    (<= (first (first rectangle)) (first point) (first (second rectangle)))
    (<= (second (first rectangle)) (second point) (second (second rectangle)))))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (first inner)) (contains-point? outer (second inner))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply >= a-seq)
    (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth (:birth-year author)
        death (:death-year author)]
    (str
      (:name author)
      (if birth
        (str " (" birth " - "
             (if death death "")
             ")")
        ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str
    (:title book)
    ", written by "
    (authors->string (:authors book))))

(defn books->string [books]
  (let [no (count books)]
    (str
      (if (== 0 no) "No" no)
      " book"
      (if (== no 1) "" "s")
      (if (> no 0) ". " "")
      (apply str (interpose ". " (map book->string books)))
      ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (not (:death-year author))) authors))

(defn has-a-living-author? [book]
  (not(empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
