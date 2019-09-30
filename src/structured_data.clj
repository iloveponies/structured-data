(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))

(defn spiff [v]
  (+ (get v 2) (get v 0)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[zx zy] point]
    (let [[[x1 y1] [x2 y2]] rectangle]
    (and (>= zx x1) (<= zx x2) (>= zy y1) (<= zy y2)))))

(defn contains-rectangle? [outer inner]
  (let [[i1 i2] inner]
    (and
     (contains-point? outer i1)
     (contains-point? outer i2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (:authors book) new-author)))

(defn alive? [author]
  (not (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (map (fn [book] (:title book)) books))

(defn monotonic? [a-seq]
  (or (apply <= (concat a-seq))
    (apply >= (concat a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(stars 5)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map (fn [book] (:authors book)) books)))

(defn all-author-names [books]
  (set (map (fn [author] (:name author)) (authors books))))

(defn author->string [author]
  (str
   (:name author)
   (if (or (:birth-year author) (:death-year author))
     (str " (" (:birth-year author) " - " (:death-year author) ")"))))

(defn authors->string [authors]
  (apply str
         (interpose ", " (map (fn [author] (author->string author)) authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (= (count books) 0) "No books."
   (= (count books) 1) (str "1 book. " (book->string (get books 0)) ".")
   (> (count books) 1) (str (apply str (count books) " books. "
                              (interpose ". " (map (fn [book] (book->string book)) books))) ".")))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
