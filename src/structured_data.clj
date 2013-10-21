(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    ))




(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y][(get v 0) (get v 2)]]
  (+ x y)
  ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x_origin y_origin]] rectangle
         [x y] point
         ]

   (and (<= x_origin x (+ (width rectangle) x_origin))
        (<= y_origin y (+ (height rectangle) y_origin))
        )
    ))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x1 y2))
         (contains-point? outer (point x2 y1))
         (contains-point? outer (point x2 y2))
         )

  ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors  (conj (:authors book) new-author)]
    (assoc book :authors authors)
    ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second_element (fn [vec] (second vec))]
    (map second_element collection)
    )
  )

(defn titles [books]
  (map :title books))


(defn monotonic? [x]
  (or (apply >= x)
      (apply <= x)
      ))

(defn stars [n]
  (apply str (repeat n "*"))

  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))


(defn contains-duplicates? [a-seq]
  (not (== (count a-seq)
           (count (set a-seq))
           )))

(defn old-book->new-book [book]
    (assoc book :authors (set (:authors book)))
    )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (fn [author] (:name author))
        years (fn [author]
                (if (contains? author :birth-year )
                 (str " (" (:birth-year author) " - " (:death-year author) ")")
                  (str "")
                  )
                )
        ]
     (str (name author) (years author) )
    )
  )

(defn authors->string [authors]
   (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string(:authors book))
  )
)

(defn books->string [books]
  (let [helper (fn [books] (str (apply str(interpose ". " (map book->string books))) "." ))]
  (cond (empty? books) "No books."
     (== (count books) 1) (apply str "1 book. " (helper books))
    :else
     (apply str (count books) " books. " (helper books))
  )
  ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
    (first (filter (fn [author] (= (:name author) name)) authors))
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not
   (empty?
    (living-authors
     (:authors book)
     )
    )
   )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
