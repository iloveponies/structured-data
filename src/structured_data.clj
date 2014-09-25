(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

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
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (=
   (height rectangle) (width rectangle)))

(defn area [rectangle]
  (*
   (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
      (and (<= x1 px x2) (<= y1 py y2)))))

(defn contains-rectangle? [outer inner]
  (let [[x1y1 x2y2] inner]
    (and
      (contains-point? outer x1y1)
      (contains-point? outer x2y2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors
      (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [col] (second col))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors
      (set authors))))

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union
    (map :authors books)))

(defn all-author-names [books]
  (set (map :name
    (authors books))))

(defn author->string [author]
  (str (get author :name)
    (if (contains? author :birth-year)
      (str " (" (get author :birth-year) " - "
        (if (contains? author :death-year)
          (str (get author :death-year) ")")
            ")")))))

(defn authors->string [authors]
  (apply str (interpose ", "
    (map author->string authors))))

(defn book->string [book]
  (apply str
    (get book :title) ", written by "
      (authors->string (get book :authors))))

(defn books->string [books]
  (let [n (count books)]
    (if (= n 0)
      "No books."
      (str n " book" (if (> n 1) "s. " ". ")
        (apply str (interpose ". "
          (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
