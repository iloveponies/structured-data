(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [v]
  (let [eka (get v 0)
        toka (get v 2)]
    (+ eka toka)))

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
  (let [[[bottom1 bottom2] [top1 top2]] rectangle]
    (- top1 bottom1)
  )
)

(defn height [rectangle]
  (let [[[bottom1 bottom2] [top1 top2]] rectangle]
    (- top2 bottom2)
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
       [point1 point2] point]
    (and (<= x1 point1 x2) (<= y1 point2 y2))
  )
)

(defn contains-rectangle? [outer inner]
  (let [[inner1 inner2] inner]
    (and (contains-point? outer inner1) (contains-point? outer inner2))
  )
)

(defn title-length [book]
  (count (get book :title))
)

(defn author-count [book]
  (count (get book :authors))
)

(defn multiple-authors? [book]
  (let [authors (author-count book)]
    (> authors 1)
  )
)

(defn add-author [book new-author]
  (let [authors (get book :authors)
        newAuthors (conj authors new-author)]
    newAuthors
    (assoc book :authors newAuthors)
  )
)

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true
  )
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [second (fn [vector] (get vector 1))]
    (map second collection)
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
  (let [seq-length (count a-seq)]
    (not= (count (set a-seq)) seq-length)
  )
)

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))
  )
)

(defn has-author? [book author]
  (contains? (get book :authors) author)
)

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(defn author->string [author]
  (let [name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)]
    (cond
      (and (nil? birth) (nil? death)) name
      (nil? death) (str name " (" birth " - )")
      :else (str name " (" birth " - " death ")")
    )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (let [title (get book :title)
        authors (get book :authors)]
    (str title ", written by " (authors->string authors))
  )
)

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (== (count books) 1) (str "1 book. " (book->string (get books 0)) ".")
    :else (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors))
)

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [authors (get book :authors)]
    (if (empty? (living-authors authors))
      false
      true
    )
  )
)

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
)

; %________%
