(ns structured-data)

(defn do-a-thing [x]
  (let [dx (+ x x)]
    (Math/pow dx dx)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

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
  (let [[[blx bly] [trx try]] rectangle]
    (- trx blx)))

(defn height [rectangle]
  (let [[[blx bly] [trx try]] rectangle]
    (- try bly)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[blx bly] [trx try]] rectangle
        [px py] point]
    (and (<= blx px trx) (<= bly py try))))

(defn contains-rectangle? [outer inner]
  (let [[ibl itr] inner]
    (and (contains-point? outer ibl) (contains-point? outer itr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (let [c (author-count book)]
    (if (> c 1)
      true
      false)))

(defn add-author [book new-author]
  (let [old-authors (:authors book)]
    (assoc book :authors (conj old-authors new-author))))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [give-second (fn [v] (get v 1))]
    (map give-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [s (set a-seq)]
    (not= (count s) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set(:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [a (str (:name author))
        p (str " (" (str(:birth-year author)) " - " )
        s (str (str(:death-year author)) ")")]
        (if (contains? author :birth-year)
          (str a p s)
          a)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [a (cond
           (== (count books) 0) "No books"
           (== (count books) 1) "1 book. "
           :else (str (count books) " books. "))]
    (str a (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (not(contains? author :death-year))) authors))

(defn has-a-living-author? [book]
  (let [alive (living-authors (:authors book))]
    (not(empty? alive))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
