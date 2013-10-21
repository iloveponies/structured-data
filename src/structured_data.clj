(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
    (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[p1 p2] rectangle]
    (- (first p2) (first p1))
   ))

(defn height [rectangle]
  (let [[p1 p2] rectangle]
    (- (second p2) (second p1))
    ))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[p1 p2] rectangle]
    (cond
     (and (<= (first p1) (first point) (first p2))
      (<= (second p1) (second point) (second p2))) true
     :else false
     )))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if
      (and (contains-point? outer p1) (contains-point? outer p2))
      true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        newp book]
    (assoc newp :authors (conj authors new-author))))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [col] (get col 1))]
    (map seconds collection)))

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
  (if (== (count (set a-seq)) (count a-seq)) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
 (if (contains? (get book :authors) author) true false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
 (str (:name author)
      (if (:birth-year author)
        (str " (" (:birth-year author) " - " (if (:death-year author) (:death-year author)) ")"
        )
      )
  )
)

(defn authors->string [authors]
  (apply str
    (interpose ", "
      (map author->string authors))))

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))))

(defn books->string [books]
  (str
   (cond
    (== (count books) 0) "No books"
    (== (count books) 1) "1 book. "
    :else (str (count books) " books. ")
    )
   (apply str (interpose ". " (map book->string books))) "."))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (if (= (filter (fn [author] (= name (:name author))) authors) ()) nil (first (filter (fn [author] (= name (:name author))) authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if
   (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
