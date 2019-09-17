(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (if (and (coll? v) (> (count v) 2))
    (+ (get v 0) (get v 2))))

(defn cutify [v]
  (if (vector? v) (conj v "<3")))

(defn spiff-destructuring [v]
(let [[x y z] v]
  (+ x z)
  ))

(defn spiff-destructuring2 [[x y z]]
  (+ x z)
  )

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
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
(let [[x3 y3] point
      [[x1 y1][x2 y2]] rectangle]
  (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  ;outer rectangle contains inner rectangle if it contains both bottom left and top right points of inner rectangle
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book) ))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [xs] (get xs 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [increasing (fn [xs] (apply <= xs))
        decreasing (fn [xs] (apply >= xs))]
    (or (increasing a-seq) (decreasing a-seq))))

(defn stars [n]
  (let [starseq (repeat n "*")]
    (apply str starseq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (= (count a-seq) (count a-set)))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [all-authors (map :authors books)]
    (apply clojure.set/union all-authors)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        author-years (if (:birth-year author)
                       (str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str author-name author-years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)
        book-list (apply str (interpose ". " (map book->string books)))]
    (str (if (= 0 book-count)
           "No books."
           (str (if (= 1 book-count) "1 book. "
                  (str book-count " books. ")) book-list ".")))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (filter (fn [author] (alive? author)) (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
