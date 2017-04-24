(ns structured-data)

(defn do-a-thing [x]
  (let [jea (+ x x)]
    (Math/pow jea jea)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[eka toka kolmas] v]
    (+ eka kolmas)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[xl] [xr]] rectangle]
    (- xr xl)))

(defn height [rectangle]
  (let [[[xl yl] [xr yr]] rectangle]
    (- yr yl)))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[xl yl] [xr yr]] rectangle
        [xp yp] point]
    (if (and (<= xl xp xr)
             (<= yl yp yr))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[li ri] inner]
    (if (and (contains-point? outer li)
             (contains-point? outer ri))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< (author-count book) 2)
    false
    true))

(defn add-author [book new-author]
  (let [auths (:authors book)]
    (assoc book :authors (conj auths new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsec (fn [v] (get v 1))]
    (map getsec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (:name author)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (authors [book]))))

(defn books->string [books]
  (cond
    (== (count books) 0) "No books."
    (== (count books) 1) (str "1 book. " (book->string (get books 0)) ".")
    :else (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))

; %________%
