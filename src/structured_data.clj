(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
)

(defn spiff [v]
  (+(get v 0) (get v 2))
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
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

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
  (== (width rectangle) (height rectangle))
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]

    (and (<= x1 px x2) (<= y1 py y2))
  )
)

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
  )
)

(defn title-length [book]
 (count (get book :title))
)

(defn author-count [book]
  (count (get book :authors))
)

(defn multiple-authors? [book]
  (> (author-count book) 1)
)

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))
  )
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [second-element (fn [v] (get v 1))]
    (map second-element collection)
  )
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
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))
    )
  )

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)
    )
  )

(defn authors [books]
  (let [authors-in-book (fn [book] (get book :authors))]
    (apply clojure.set/union (map authors-in-book books))
  )
)

(defn all-author-names [books]
  (let [name-of-author (fn [author] (get author :name))]
    (set (map name-of-author (authors books)))
  )
)

(defn author->string [author]
  (apply str(cond
              (contains? author :death-year) (str (get author :name) " (" (get author :birth-year) " - " (get author :death-year) ")")
              (contains? author :birth-year) (str (get author :name) " (" (get author :birth-year) " - )")
              :else                          (str (get author :name))
            )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )


(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors)))
)

(defn books->string [books]
  (cond
    (== (count books) 0)  
      (str "No books.")
    
    (== (count books) 1)  
      (str "1 book. " (book->string(get books 0)) ".")

    (>= (count books) 2)  
      (str (count books) " books. " 
           (apply str(interpose ". " (map book->string books))) ".")
  )
)

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books)
  )

(defn author-by-name [name authors]
    (first (filter (fn [a] (==(compare (get a :name) name) 0)) authors))
)

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors))))
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
  )

; %________%
