(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v] (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (if (> 0 (- x1 x2)) (- (- x1 x2)) (- x1 x2))
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (if (> 0 (- y1 y2)) (- (- y1 y2)) (- y1 y2))
    ))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
  (and (<= x1 p1 x2) (<= y1 p2 y2))
    ))

(defn contains-rectangle? [outer inner]
  (let [[i1 i2] inner]
  (and (contains-point? outer i1) (contains-point? outer i2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [length (fn [x](count x))]
    (map length (seq collection))
    ))

(defn second-elements [collection]
  (let [vectors (fn [x](get x 1))]
    (map vectors (seq collection))
    ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [setti (set a-seq)
        tg (fn [x]( (toggle setti x) ))
        filttered (map tg a-seq)]
    (not(= (count a-seq) (count setti)))
    )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authorsbb (fn [book] (:authors book))]
    (apply clojure.set/union (map authorsbb books))))

(defn all-author-names [books]
    (set(map :name (authors books))))

(defn author->string [author]
  (cond
    (contains? author :death-year) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (contains? author :birth-year) (str (:name author) " (" (:birth-year author) " - " ")")
    :else (str (:name author))))

(defn authors->string [authors]
  (let [stringify
         (fn [author](str (author->string author)))]
  (apply str (interpose ", " (map stringify authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (authors [book]))))

(defn books->string [books]
  (cond
    (= 0 (count books)) "No books."
    (= 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
    :else (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; '_______'
