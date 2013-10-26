(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]

    (Math/pow x2 x2)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3" ))

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
    (== (- x1 x2)(- y1 y2))
    )
  )

(defn area [rectangle]
    (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1][rx2 ry2]] rectangle
        [px py] point ]
    (and (<= rx1 px rx2) (<= ry1 py ry2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner]
    (and
     (contains-point? outer p1)
     (contains-point? outer p2)
     )
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
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [element] (count element))
       collection
   )
  )

(defn second-elements [collection]
  (let [second (fn [collection] (get collection 1))]
    (map second collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (if (apply <= a-seq)
    true
    (apply >= a-seq)
    )
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
  (not (== (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  (assoc book :authors  (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (apply
    clojure.set/union
    (map :authors books)
   )
 )

(defn all-author-names [books]
  (set
    (concat
      (map :name (authors books))
     )
   )
 )

(defn author->string [author]
  (let [name (fn [author] (:name author))
        years (fn [author]
                (if (contains? author :birth-year)
                  (str
                   " ("
                   (:birth-year author)
                   " - "
                   (if (contains? author :death-year)
                     (:death-year author)
                     ""
                     )
                   ")"
                   )
                   ""
                  )
                )
        ]
     (str (name author) (years author))
   )
  )


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
 )

(defn book->string [book]
   (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (let [book-list (fn [books] (apply str (interpose ", " (map book->string books))))]
    (cond
      (== 0 (count books)) "No books."
      (== 1 (count books)) (str "1 book. " (book->string(get books 0)) ".")
      (<  1 (count books)) (str (count books) " books. " (book-list books) ".")
     )
   )
 )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
  )

(defn living-authors [authors]
  (filter
   (fn [author] (not(contains? author :death-year))
    )
   authors
   )
 )

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
