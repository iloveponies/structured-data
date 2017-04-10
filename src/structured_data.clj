(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)
  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
  )
)

(defn point [x y]
  [x y]
)

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]
)

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
  )
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
  )
)

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x1 x2) (- y1 y2))
  )
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))
  )
)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (< 1 (author-count book))
)

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author))
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
      (map get-second collection)
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n \*))
)

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (not (apply distinct? a-seq))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors)))
)

(defn has-author? [book author]
  (contains? (book :authors) author)
)

(defn authors [books]
  (let [author (fn [book] (:authors book))]
    (set (apply concat (map author books))))
)

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(defn author->string [author]
  (cond
    (author :death-year) (str (author :name) " (" (author :birth-year) " - " (author :death-year) ")")
    (author :birth-year) (str (author :name) " (" (author :birth-year) " - )")
    :else (author :name)
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (apply str (book :title) ", written by " (authors->string (book :authors)))
)

(defn books->string [books]
  (let [list-books (fn [books]
    (apply str (interpose ", " (map book->string books))))]

    (cond
      (= 1 (count books)) (str (count books) " book. " (list-books books) ".")
      (< 1 (count books)) (str (count books) " books. " (list-books books) ".")
      :else "No books."
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (x :name) name)) authors))
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
  (< 0 (count (filter alive? (book :authors))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
