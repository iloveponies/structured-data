(ns structured-data)

(defn do-a-thing [x]
  (let [ base (+ x x)
         head (+ x x)]
         (Math/pow base head)))

(defn spiff [v]
  (+
    (get v 0)
    (get v 2)))

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
    (=
      (width rectangle)
      (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
        (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[pointX pointY] inner]
        (and
          (contains-point? outer pointX)
          (contains-point? outer pointY))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [el (fn [x] (get x 1))]
  (map el collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
  (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
  (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author (fn [book] (:authors book))]
    (apply clojure.set/union (map author books))))

(defn all-author-names [books]
    (set (map :name (authors books))))

(defn author->string [author]
  (let [years (fn [auth] (cond
    (contains? auth :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
    (contains? auth :birth-year) (str " (" (:birth-year author) " - )")
    :else (str "")))]
    (str (:name author) (years author))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookcount (count books)
        authors (apply str (interpose ". " (map book->string books)))]
  (cond
    (= bookcount 0) (str "No books.")
    (= bookcount 1) (str "1 book. " authors ".")
    :else (str bookcount " books. " authors ".")
    )))

(defn books-by-author [author books]
  (filter
    (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first
    (filter
      (fn [author] (= (:name author) name))
      authors )))

(defn living-authors [authors]
  (set
    (filter
      (fn [author] (alive? author))
      authors )))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (filter alive? authors)))))

(defn books-by-living-authors [books]
  (set
    (filter
      (fn [book] (has-a-living-author? book))
      books )))

; %________%
