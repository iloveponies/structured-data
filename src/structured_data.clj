(ns structured-data)

(defn do-a-thing [x]
  (let
    [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x0 x1 x2] v]
    (+ x0 x2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (== w h)
    )
  )

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)
    )
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))


(defn contains-rectangle? [outer inner]
  (let [[x1 x2] inner]
    (and (contains-point? outer x1)
         (contains-point? outer x2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)
        authors2 (conj authors new-author)]
    (assoc book :authors authors2)
    ))


(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secondF (fn [col] (get col 1))]
    (map secondF collection)))

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
    (if (== (count a-seq) (count a-set)) false true)))

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
        b-year (:birth-year author)
        d-year (:death-year author)]
    (if (boolean b-year)
      (str name " (" b-year " - " d-year ")")
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [authors (authors->string (:authors book))]
    (str (:title book) ", written by " authors)))

(defn books->string [books]
  (let [count (count books)
        bookPref (if (== count 1) " book. " " books. ")
        prefix (str count bookPref)
        bookStr (apply str (interpose ". " (map book->string books)))]
    (if (== count 0)
      "No books."
      (str prefix bookStr "."))))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter
           (fn [a] (= name (:name a)))
           authors)))

(defn living-authors [authors]
  (filter (fn [a] (alive? a)) authors))

(defn has-a-living-author? [book]
  (let [living (living-authors (:authors book))]
    (if (empty? living) false true)))

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))

; %________%
