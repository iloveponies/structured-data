(ns structured-data)

(defn do-a-thing [x]
  (let [result (+ x x) ]

    (Math/pow result result)

  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x z]  [(get v 0) (get v 2)]]
   (+ x z)
   )
  )



(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ) )




(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (- y2 y1)
    )

  )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]

    (== (height rectangle) (width rectangle))

    )

  )

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (height rectangle) (width rectangle))
    )


  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
    ( and
      (<= x1 px)
      (>= x2 px)
      (<= y1 py)
      (>= y2 py)
    )
    )
  )

  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and
     (contains-point? outer [x1 y1])
     (contains-point? outer [x2 y2])
     )
  )
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)
 )
)

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)

  )

(defn second-elements [collection]
(let [eka (fn [s] (get s 1))]
    (map eka collection)
    ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]

  (or (apply <= a-seq) (apply >= a-seq) )

  )

(defn stars [n]

(apply str (apply concat (repeat n  "*")))

  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )

  )


(defn contains-duplicates? [a-seq]
  (let [[xx] [(set a-seq)]]
    (> (count a-seq) (count xx))
    ))


(defn old-book->new-book [book]
  (assoc book :authors  (set (book :authors)))
  )

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]

    (set (apply clojure.set/union (map :authors books)))

  ;(apply clojure.set/union (books :authors))

  )

(defn all-author-names [books]
  (let [author-names
        (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books))))
  )

(defn author->string [author]

  (let [[name] [(str (author :name))]]
    (let [[by] [(str (author :birth-year))]]
      (let [[dy] [(str (author :death-year))]]
        (if (contains? author :birth-year)
        (str name " (" by " - " dy ")")
        (str name)
        )
      )
    )
  )
)


(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [[book-name] [ (str (:title book))]]
   (str book-name ", written by " (authors->string (:authors book)))
  )
)


(defn books->string [books]
  (cond
   (empty? books) "No books."
   (== (count books) 1) (str "1 book. " (apply str (map book->string books)) ".")
   :else (str (count books) " books. " (apply str (interpose ". "(map book->string books))) ".")
   ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)

  )

(defn author-by-name [name authors]

  (first (filter (fn [n] (= (:name n) name)) authors))

  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
  )

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0)
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)

  )

; %________%
