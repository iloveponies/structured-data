(ns structured-data)

(defn do-a-thing [x]
  (let [xyz (+ x x)]
  (Math/pow xyz xyz)
  ))

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
   (let [[[x y] [a b]] rectangle]
     (- a x)
     )
  )

(defn height [rectangle]
  (let [[[x y] [a b]] rectangle]
     (- b y)
     )
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
)


(defn area [rectangle]
  (let [[[x y][a b]] rectangle]
    (* (width rectangle) (height rectangle))
    )
)


(defn contains-point? [rectangle point]
  (let [[[x y][a b]] rectangle]
    (let [[p1 p2] point]
      (and (<= x p1 a) (<= y p2 b))
    )
  )
)


(defn contains-rectangle? [outer inner]
  (let [[f s] inner]
    (and (contains-point? outer f) (contains-point? outer s))
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
  (= nil (:death-year author))
  )


(defn element-lengths [collection]
  (map (fn len [x] (count x)) collection)
  )

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection)
  )

(defn titles [books]
  (map :title books)
  )


(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq))
  )

(defn stars [n]
  (apply str(repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq)))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (author :name)]
    (let [years (str "(" (author :birth-year) " - " (author :death-year) ")")]
      (str name (if (contains? author :birth-year) (str " " years) ""))
    )
  )
)


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors)))
)

(defn books->string [books]
  (if (= 0 (count books))
    "No books."
    (if (= 1 (count books))
      (str (count books) " book. " (apply str (interpose ". " (map book->string books))) ".")
      (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books )
  )

(defn author-by-name [name authors]
  (let [name (filter (fn [x] (= (x :name) name)) authors)]
    (if (= (count name) 0)
      nil
      (first name)
      )
    )
  )


(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
  )


(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors))))
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )



; %________%
