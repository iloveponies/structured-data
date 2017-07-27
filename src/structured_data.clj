(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)
    ))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [derp (fn [x] (get x 1))]
  (map derp collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)(apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
     (disj a-set elem)
     (conj a-set elem)))

(defn contains-duplicates? [a-seq]
 (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (str (:name author))
    year (str " ("(:birth-year author) " - " (:death-year author) ")")]
    (if (contains? author :birth-year)
    (str name year)
    (str name))))

(defn authors->string [authors]
  (apply
    str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (str
    (:title book) ", written by "
    (authors->string (:authors book))))

(defn books->string [books]
  (let [amount (count books)
       booklist (apply str(interpose ". " (map book->string books)))]
       (cond
        (= 1 amount) (str "1 book. " booklist ".")
        (= 0 amount) (str "No books.")
       :else (str amount " books. " booklist "."))))

(defn books-by-author [author books]
  (filter
    (fn [x] (has-author? x author))
    books))

(defn author-by-name [name authors]
  (first
    (filter
      (fn [x] (= name (:name x)))
      authors)))

(defn living-authors [authors]
  (filter
    (fn [x] (alive? x))
    authors))

(defn has-a-living-author? [book]
  (if
    (empty?
      (living-authors (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter
    (fn [x]
      (has-a-living-author? x))
    books))

; :-)
