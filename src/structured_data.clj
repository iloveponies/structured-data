(ns structured-data)

(defn do-a-thing [x]
  (let [dbl (+ x x)]
    (Math/pow dbl dbl)))

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
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (Math/abs (- x1 x2))
           (Math/abs (- y1 y2)))
      true
      false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (Math/abs (- x1 x2))
       (Math/abs (- y1 y2)))))

(defn contains-point? [rectangle point]
  (let [[x y] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[i1 i2] inner]
    (and (contains-point? outer i1)
         (contains-point? outer i2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (apply >= a-seq)
    true
    (apply <= a-seq)))

(defn stars [n]
  (if (= n 1)
    "*"
    (str "*" (stars (- n 1)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (set (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years (fn [author]
                (if (:birth-year author)
                  (str " (" (:birth-year author) " - " (:death-year author) ")")
                  ""))]
    (str (:name author) (years author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [prefix (cond (= (count books) 0) "No books"
                     (= (count books) 1) "1 book. "
                     :else (apply str (count books) " books. "))]
    (apply str prefix (apply str (interpose ". " (map book->string books))) ".")))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter
    (fn [author]
      (= (:name author) name))
    authors)))

(defn living-authors [authors]
  (filter (fn [author]
            (alive? author))
          authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book]
            (has-a-living-author? book))
          books))

; %________%
