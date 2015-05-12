(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (if (< (count v) 3)
    \?
    (+ (get v 0) (get v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (height rectangle) (width rectangle))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (height rectangle) (width rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (>= x x1) (<= x x2) (>= y y1) (<= y y2))))

(defn contains-rectangle? [outer inner]
  (let [[a b] inner]
    (and (contains-point? outer a) (contains-point? outer b))))

(defn title-length [book]
   (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [updated (conj (:authors book) new-author)]
    (assoc book :authors updated)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [element] (get element 1))]
    (map second collection)))

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
  (let [updated (set (:authors book))]
    (assoc book :authors updated)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
 (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [year-str (if (contains? author :birth-year)
                   (str " (" (:birth-year author) " - " (:death-year author) ")")
                   nil)]
    (str (:name author) year-str)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [count-str (cond
                   (== (count books) 1) "1 book. "
                   (> (count books) 1) (str (count books) " books. ")
                   :else  "No books")]
    (str count-str (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
