(ns structured-data)

(defn do-a-thing [x]
  (let [xplusx (+ x x)]
    (Math/pow xplusx xplusx)))

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
  (let [[[x1] [x2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc
    book
    :authors
    (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [get-second (fn [vec] (get vec 1))]
    (map get-second collection)))

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
  (let [name (:name author)
        from-till (if (contains? author :birth-year)
                    (str " ("
                       (:birth-year author)
                       " - "
                       (:death-year author)
                       ")")
                    "")]
    (str name from-till)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-title (:title book)
        written-by (authors->string (:authors book))]
    (str book-title ", written by " written-by)))

(defn books->string [books]
  (let [prefix (cond
                (= (count books) 0) "No books"
                (= (count books) 1) "1 book. "
                :else (str (count books) " books. "))
        book-names (if (> (count books) 0)
                       (apply  str (interpose ". " (map book->string books)))
                       "")]
    (str prefix book-names ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty?(living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
