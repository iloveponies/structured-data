(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
    (Math/pow x+x x+x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z))
  )

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 x2] [y1 y2]] [z1 z2]]
  (and (<= x1 z1 y1) (<= x2 z2 y2))
  )

(defn contains-rectangle? [outer [[x1 y1] [x2 y2]]]
  (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2])))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (:authors book)
        add-authors (conj authors new-author)]
    (assoc book :authors add-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors-as-set (set (:authors book))]
    (assoc book :authors authors-as-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birthyear (:birth-year author)
        deathyear (:death-year author)]
    (if (contains? author :birth-year)
      (str name " (" birthyear " - " deathyear ")")
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        author (authors->string (:authors book))]
    (str title ", written by " author)))

(defn books->string [books]
  (let [nrBooks (count books)
        booksString (apply str (interpose ". " (map book->string books)))]
    (cond
      (== 0 nrBooks) (str "No books.")
      (== 1 nrBooks) (str nrBooks " book. " booksString ".")
      :else (str nrBooks " books. " booksString "."))))



(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
