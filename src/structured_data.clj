(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)] (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x0 x1 x2] v] (+ x0 x2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (>= (author-count book) 2))

(defn add-author [book new-author]
  (let [newAuthors (conj (book :authors) new-author)]
    (assoc book :authors newAuthors)))

(defn alive? [author]
(not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [dummy (fn [x] (get x 1))]
    (map dummy collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))


(defn old-book->new-book [book]
  (let [authorsSet (set (:authors book))]
    (assoc book :authors authorsSet)))


(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if (not (contains? author :birth-year)) name
        (str name " (" birth " - " death ")"))))

(defn authors->string [authors]
  (apply str (interpose ", "
                        (map author->string authors))))


(defn book->string [book]
  (let [authorNames (map author->string (:authors book))
        bookName (:title book)]
    (str bookName ", written by "
         (apply str (interpose ", " authorNames)))))


(defn books->string [books]
  (cond
   (== (count books) 0) "No books."
   (== (count books) 1) (str "1 book. " (apply str (map book->string books)) ".")
   :else (str (count books)
              " books. " (apply str (interpose ". "
                                              (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (or (has-author? x author))) books))

(defn author-by-name [namex authors]
  (first (filter (fn [x] (= namex (:name x))) authors)))


(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))


; %________%
