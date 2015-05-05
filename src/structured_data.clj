(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [f (get v 0)
        t (get v 2)]
    (+ f t)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1][x2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1][_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[i1 i2] inner]
    (and (contains-point? outer i1) (contains-point? outer i2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [orig (:authors book)
        new (conj orig new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secs (fn [c] (get c 1))]
    (map secs collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [incr (apply <= a-seq)
        decr (apply >= a-seq)]
    (or incr decr)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [c1 (count a-seq)
        c2 (count (set a-seq))]
    (not (= c1 c2))))

(defn old-book->new-book [book]
  (let [s (set (:authors book))]
    (assoc book :authors s)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (if (:birth-year author)
      (str name " " years)
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [c (count books)
        b (map book->string books)]
    (str (cond
           (< c 1) "No books"
           (= c 1) (str c " book. " (apply str b))
           (> c 1) (str c " books. " (apply str (interpose ", " b))))
         ".")))

(defn books-by-author [author books]
  (let [pred (fn [bs] (has-author? bs author))]
    (filter pred books)))

(defn author-by-name [name authors]
  (let [name? (fn [author] (= (:name author) name))]
    (first (filter name? authors))))

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
