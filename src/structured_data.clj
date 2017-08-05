(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
  (Math/pow y y))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
  ))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 (get point 0) x2) (<= y1 (get point 1) y2))
    ))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
      (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))
  ))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (defn length-element [x]
    (count x))
    (map length-element collection)
  )

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
(assoc book :authors (set (get book :authors)))
  )

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [authors (fn [book] (:authors book))]
    (apply clojure.set/union (map authors books))
    )
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(defn author->string [author]
  (let [name (:name author)
  years (cond
    (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
    (contains? author :birth-year) (str " (" (:birth-year author) " - )")
  )]
  (str name years))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)
(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (== 0 (count books)) (str "No books.")
    (== 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
  (let [author (filter (fn [author] (= name (:name author))) authors)]
  (first author)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
  )

(defn has-a-living-author? [book]
  (not(empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )

; %________%
