(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y))
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v] (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x_bl y_bl] [x_tp y_tp]]]
  (- x_tp x_bl)
  )

(defn height [[[x_bl y_bl] [x_tp y_tp]]]
  (- y_tp y_bl)
  )

(defn square? [rectangle]
  (== (height rectangle) (width rectangle))
)

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
)

(defn contains-point? [[[x_bl y_bl] [x_tp y_tp]] [p1 p2]]
  (and (<= x_bl p1 x_tp) (<= y_bl p2 y_tp))
  )

(defn contains-rectangle? [outer inner]
  (let [[bl tp] inner]
    (and (contains-point? outer bl) (contains-point? outer tp)))
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false)
  )

(defn add-author [book new-author]
  (let [title (:title book)
        authors (:authors book)]
    (assoc book :authors (conj authors new-author)))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection))
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true)
  )

(defn old-book->new-book [book]
  (let [old-authors-list (:authors book)
       new-authors-set (set old-authors-list)]
      (assoc book :authors new-authors-set))
  )

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        birth-str (if birth-year (str " (" birth-year " - ") "")
        death-str (if birth-year (str death-year ")") "")]
    (str name birth-str death-str))
  )

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (let [num-books (count books)
        total-str (cond (== num-books 0) "No books"
                        (> num-books 1) (str num-books " books. ")
                        :else "1 book. ")]
    (str total-str (apply str (interpose ". " (map book->string books))) ".")
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (some alive? (:authors book)) true false))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
