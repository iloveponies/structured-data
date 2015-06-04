(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[a b] point]
      (and (<= x1 a x2) (<= y1 b y2)))))

(defn contains-rectangle? [outer inner]
  (let [[little bigger] inner]
    (and (contains-point? outer little) (contains-point? outer bigger))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (not (= 1 (author-count book))))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count (seq collection)))

(defn second-elements [collection]
  (seq (map first (map rest collection))))

(defn titles [books]
  (seq (map :title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
  years (str " (" (:birth-year author) " - " (:death-year author) ")")]
  (if (:birth-year author) (str name years) (str name))))

(defn authors->string [authors]
  (let [moby (map author->string authors)]
    (if (>= (count authors) 2)
      (apply str (interpose ", " moby))
      (apply str moby))))

(defn book->string [book]
  (if (:authors book)
    (apply str (:title book) ", written by " (authors->string (:authors book)))
    (apply str (:title book))))

(defn books->string [books]
  (if (= 0 (count books))
    "No books."
    (if (= 1 (count books))
      (apply str "1 book. " (apply book->string books))
      (apply str (count books) " books. " (interpose ". " (map book->string books))))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (seq (filter (fn [x] (= (:name x) name)) (seq authors))))

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
; (use 'structured-data :reload)
; (source fn), prints the source of function