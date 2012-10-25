(ns structured-data)

(defn do-a-thing [x]
  (let [xx (* x x)])
  (Math/pow (+ x x) (+ x x)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (let [[xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])) 
         (contains-point? outer [x2 y2])))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [length (fn [x] (count x))]
    (map length collection)))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)))

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
  (assoc book :authors
  	(set (:authors book))))

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
        auth (authors->string (:authors book))]
    (str name ", written by " auth)))

(defn books->string [books]
  (let [amount (count books)
        description (apply str (interpose ". " (map book->string books)))]
    (if (== amount 0) "No books."
      (if (== amount 1) (str "1 book. " description ".")
        (str amount " books. " description ".")))))

(defn books-by-author [author books]
  (let [auth (fn [book] (has-author? book author))]
    (filter auth books)))

(defn author-by-name [name authors]
  (let [finder (fn [author] (= name (:name author)))
        found (filter finder authors)]
    (if (empty? found) nil
      (first found))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))