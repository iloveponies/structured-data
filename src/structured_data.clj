(ns structured-data)

(defn abs [x]
  (if (< x 0) (- x) x))

(defn do-a-thing [x]
  (let [sumx (+ x x)]
    (Math/pow sumx sumx)))

(defn spiff [v]
  (let [size (count v)]
    (if (< size 1)
      nil
      (if (< size 3)
        (get v 0)
        (+ (get v 0) (get v 2))))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [size (count v)
        [x y z] v]
    (if (< size 1)
      nil
      (if (< size 3) x (+ x z)))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- x2 x1)))

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2]))))

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
  (let [secnd (fn [[x y]] y)]
    (map secnd collection)))

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
  (not (== (count a-seq) (count (set a-seq)))))

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
        died (if (:death-year author) (str (:death-year author)) "")
        years (if (:birth-year author) (str " (" (:birth-year author) " - " died ")") "")]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [name (:title book)
        auths (authors->string (:authors book))]
    (str name ", written by " auths)))

(defn books->string [books]
  (let [amount (count books)
        descriptions (apply str (interpose ". " (map book->string books)))]
    (if (< amount 1) "No books."
      (if (== amount 1) (str "1 book. " descriptions ".")
        (str amount " books. " descriptions ".")))))

(defn books-by-author [author books]
  (let [func (fn [book] (has-author? book author))]
    (filter func books)))

(defn author-by-name [name authors]
  (let [func (fn [author] (= name (:name author)))
        found (filter func authors)]
  	(if (empty? found) nil
      (first found))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))