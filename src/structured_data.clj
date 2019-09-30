(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1, y1] [x2, y2]]]
  (- x2 x1))

(defn height [[[x1, y1] [x2, y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1, y1] [x2, y2]] [x3, y3]]
  (and (<= x1 x3 x2) (<= y1 y3 y2)))

(defn contains-rectangle? [outer [x y]]
  (and (contains-point? outer x) (contains-point? outer y)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [xs (:authors book)]
    (assoc book :authors (conj xs new-author))))

(defn alive? [author]
  (if (:death-year author) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

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
  (str (:name author)
       (if (:birth-year author)
         (str " (" (:birth-year author) " - "
              (if (:death-year author) (:death-year author) "") ")") "")))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
 (str (if (== 0 (count books)) "No books."
        (str (count books)
         (if (< 1 (count books)) " books. " " book. ")
         (apply str (interpose ". " (map book->string books)))
         ".")
        )))

(defn books-by-author [author books]
  (filter (fn [b] (contains? (:authors b) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (:name a))) authors)))

(defn living-authors [authors]
  (filter (fn [a] (not (boolean (:death-year a)))) authors))

(defn has-a-living-author? [book]
  (boolean (first (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
