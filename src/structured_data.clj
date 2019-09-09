(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    )
  )

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[x y] point [[x1 y1] [x2 y2]] rectangle]
        (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[tl br] inner]
    (and (contains-point? outer tl) (contains-point? outer br))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secfn (fn [x] (get x 1))]
    (map secfn collection)))

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
  (let [a-set (set a-seq)]
    (if (= (count a-set) (count a-seq))
      false
      true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        lifespan (cond
                  (and birth-year death-year) (str " (" birth-year " - " death-year ")")
                  birth-year (str " (" birth-year " - )")
                  :else "") ]
    (str name lifespan)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [nbr-books (count books)
        book-names (apply str (interpose ". " (map book->string books)))]
    (cond
     (== nbr-books 1) (str "1 book. " book-names ".")
     (> nbr-books 1) (str nbr-books " books. " book-names ".")
     :else "No books.")))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (let [ff (fn [a] (= name (:name a)))]
    (first (filter ff authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
