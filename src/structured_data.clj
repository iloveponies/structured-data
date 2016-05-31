(ns structured-data)

(defn do-a-thing [x]
  (let [doubled (+ x x)]
    (Math/pow doubled doubled)))

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
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[x0 y0] point]
    (let [[[x1 y1] [x2 y2]] rectangle]
      (and (<= x1 x0 x2) (<= y1 y0 y2)))))

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
  (let [newlist (conj (book :authors) new-author)]
     (assoc book :authors newlist)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get1 (fn [x] (get x 1))]
    (map get1 collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond (apply <= a-seq) true
        (apply >= a-seq) true
        :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authorset (set (book :authors))]
    (assoc book :authors authorset)))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (let [authorset
         (fn [book] (book :authors))]
    (apply clojure.set/union (map authorset books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond
    (contains? author :death-year) (str (author :name) " (" (author :birth-year) " - " (author :death-year) ")")
    (contains? author :birth-year) (str (author :name) " (" (author :birth-year) " - )")
    :else (author :name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors))))

(defn books->string [books]
  (let [amount (count books)]
    (let [start
      (cond
        (= amount, 0) "No books"
        (= amount, 1) "1 book. "
        :else (str amount " books. "))]
      (let [end
        (apply str (interpose ", " (map book->string books)))]
      (str start end ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (author :name) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (not (contains? author :death-year))) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
