(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let[[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let[[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [pointX pointY] point]
    (and (<= x1 pointX x2) (<= y1 pointY y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]) )))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [newAuthors (conj (get book :authors) new-author)
    newBook (assoc book :authors newAuthors)]
    newBook))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (let [getLength (fn [x] (count x))]
    (map getLength collection)))

(defn second-elements [collection]
  (let [getSecond (fn [x] (get x 1))]
    (map getSecond collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply >= a-seq) true
    (apply <= a-seq) true
    :else false
    ))

(defn stars [n]
  (apply str (repeat n "*") ))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (> (count a-seq) (count (set a-seq)))
    true
    false))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [alku (:name author)
    loppu 
    (cond 
      (not (nil? (:death-year author))) (str " (" (:birth-year author) " - " (:death-year author) ")")
      (not (nil? (:birth-year author))) (str " (" (:birth-year author) " - )")
      :else "")]
    (str alku loppu)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (if (not (nil? (:authors book)))
    (str (:title book) ", written by " (authors->string (:authors book)))
    (str (:title book))))

(defn books->string [books]
  (let [alku 
    (cond
      (< 1 (count books)) (str (count books) " books. ")
      (= 1 (count books)) "1 book. "
      :else "No books")]
    (str alku (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%

