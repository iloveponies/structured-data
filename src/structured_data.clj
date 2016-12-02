















(ns structured-data)

(defn do-a-thing [x]
  (let [xx (* 2 x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)]
  (let [y (get v 2)]
  (+ x y))))


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
    (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
      (and (<= x1 x x2) (<= y1 y y2)))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))



(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))


(defn alive? [author]
  (not(contains? author :death-year)))


(defn element-lengths [collection]
  (let [length (fn [x] (count x))]
    (map length collection)))

(defn second-elements [collection]
    (let [add (fn [x] (get x 1))]
      (map add collection)))

(defn titles [books]
  (let [title (fn [x] (:title x))]
    (map title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not(= (count (set a-seq)) (count a-seq))))


(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))


(defn has-author? [book author]
  (contains? (:authors book) author))


(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name-str (str (:name author))]
    (let [year-str  (str " (" (str (:birth-year author)) " - " (str (:death-year author)) ")")]
      (if (:birth-year author) (str name-str year-str) (str name-str)))))


(defn authors->string [authors]
  (let [x (interpose ", "  (map author->string authors))]
    (apply str x)))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (cond
   (= 0 (count books)) (str "No books.")
   (= 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
   :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))


(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))


(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))


(defn has-a-living-author? [book]
  (not (empty? (filter (fn [x] (alive? x)) (:authors book)))))


(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))


; %________%
