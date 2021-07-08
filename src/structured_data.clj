(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
  )

(defn spiff [v]
  (let [firstElem (get v 0)
        secondElem (get v 2)]
    (cond (nil? secondElem) firstElem
          :else (+ firstElem secondElem)
          )
  )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)
  )
  )

(defn square? [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (== (- x1 x2) (- y1 y2))
    )
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle)
  )
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [px1 py1] point]
    (and (<= x1 px1 x2) (<= y1 py1 y2))
    )
)

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))
    )
)

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )



(defn multiple-authors? [book]
  (> (author-count book) 1)
)
(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year ))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [take-second (fn [subcollection] (get subcollection 1))]
    (map take-second collection)
  )
)

(defn titles [books]
  (let [get-title (fn [book] (:title book))]
    (map get-title books)
   ))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)
  )
  )

(defn stars [n]
  (reduce str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not(= (count a-set) (count a-seq)))
    )
  )

(defn old-book->new-book [book]
  (let [authors (:authors book)]
     (assoc book :authors (set authors))
   )
  )

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)
    )
)

(defn authors [books]
   (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
        years
                (cond (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
                      (contains? author :birth-year) (str " (" (:birth-year author) " - )")
                       :else ""
                      )
        ]
    (str name years)
    )
)

(defn authors->string [authors]
  (let [transformedAuthors (map author->string authors)]
      (apply str (interpose ", " transformedAuthors) )
    )
)

(defn book->string [book]
  (let [title (:title book)
        author-names (authors->string (authors [book]))
        ]
    (str title ", written by " author-names)
  )
  )

(defn books->string [books]
  (let [bookcount (count books)
        converted-books (apply str (interpose ", " (map book->string books)))]
  (cond (= 0 bookcount) "No books."
        (= 1 bookcount) (str "1 book. " converted-books ".")
        :else (str bookcount " books. " converted-books ".")
  )
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
  )

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
  (not (empty? (living-authors authors)))
  )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)
