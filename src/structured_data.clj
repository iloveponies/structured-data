(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (== (height rectangle) (width rectangle))))

(defn area [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (* (height rectangle) (width rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x3 y3][x4 y4]] inner]
    (cond
      (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4])) true
      :else false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1)
    true
    false))


(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (:death-year author)
    false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
  (let [repeated (repeat n "*")]
    (apply str repeated)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [set1 (set a-seq)]
    (if (== (count a-seq) (count set1))
      false
      true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
   (apply clojure.set/union (map :authors books )))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        byear (:birth-year author)
        dyear (:death-year author)]
    (if (not byear)
      (str name)
      (str name " (" byear " - " dyear ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [number (count books)]
    (cond
      (== number 0) (str "No books.")
      (== number 1) (str number " book. " (apply str (map book->string books)) ".")
      :else (str number " books. " (apply str (interpose ", " (map book->string books))) "."))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books) )

(defn author-by-name [name authors]
  (let [has-author (fn [author] (= (:name author) name))]
    (first (filter has-author authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
