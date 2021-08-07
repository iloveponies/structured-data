(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (first v) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[one _ three] v]
    (+ one three)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [[[x1 y1] [x2 y2]]]
  (if (=
        (- x2 x1)
        (- y2 y1))
    true false))

(defn area [[[x1 y1] [x2 y2]]]
  (let [length (- x2 x1)
        width (- y2 y1)]
    (* length width)))

(defn contains-point? [[[x1 y1] [x2 y2]] [z1 z2]]
  (if (and
        (>= x2 z1 x1)
        (>= y2 z2 y1)) true false))

(defn contains-rectangle? [outer [[x1 y1] [x2 y2]]]
  (if (and
        (contains-point? outer [x1 y1])
        (contains-point? outer [x2 y2]))
    true false))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  ;(map second collection)
  (let [second-ele (fn [x] (second x))]
    (map second-ele collection))
  )

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
  (let [a-seq-count (count a-seq)
        a-seq-set-count (count (set a-seq))]
    (not (= a-seq-count a-seq-set-count))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [book-authors (map :name (:authors book))
        author-name (:name author)]
    (if (some #(= author-name %) book-authors) true false)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (apply clojure.set/union (map :authors books)))))

(defn author->string [author]
  (if (:birth-year author)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (:name author)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (= 0 (count books)) "No books."
    (= 1 (count books))
      (str "1 book. " (book->string (first books)) ".")
    (> (count books) 1 )
      (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  ;(first (filter (fn [author] (= name (:name author))) authors))
  (first (filter #(= name (:name %)) authors))
  )

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (let [book-authors (:authors book)]
    (not (empty? (living-authors book-authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
