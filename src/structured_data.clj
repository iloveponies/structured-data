(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
)

(defn spiff [v]
  (let [first (get v 0)
        third(get v 2)]
    (+ first third)
   )

)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[first s third] v]
    (+ first third)
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
  (let [ w (width rectangle)
         h (height rectangle)]
    (cond
      (= w h) true
      :else false
    )
  )
)

(defn area [rectangle]
  (let [ w (width rectangle)
         h (height rectangle)]
    (* w h)
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point
        ]
    (if (and (<= x1 x x2)(<= y1 y y2))
      true

      false
    )
  )

)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]

    (if (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))
      true

      false
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
  (cond
    (> (author-count book) 1) true
    :else false
  )
)

(defn add-author [book new-author]
  (let [authors (get book :authors)
        new (conj authors new-author)
        ]

    (assoc book :authors new)
  )
)

(defn alive? [author]
  (cond
    (contains? author :death-year) false
    :else true

  )
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [find-seconds(fn [vector] (get vector 1))]
    (map find-seconds collection)
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false
  )
)

(defn stars [n]

  (loop [stars "" x n]
    (if (= x 0)
      stars

      (recur (str stars "*") (- x 1))
    )
  )


)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)

    (conj a-set elem)

  )
)

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if(< (count a-set) (count a-seq))
      true

      false
    )
  )
)

(defn old-book->new-book [book]
  (let [new-book book auths (set (:authors book))]
    (assoc new-book :authors auths)
  )
)

(defn has-author? [book author]
  (let [auths (:authors book)]
    (contains? auths author)
  )
)

(defn authors [books]
  (set(apply clojure.set/union (map :authors books)))

)

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(defn author->string [author]
  (let [death (:death-year author) birth (:birth-year author ) name (:name author)]
    (if (= death nil)
      (if (= birth nil)
        name
        (str name " ("birth " - )" )
      )

      (str name " ("birth " - " death")" )
    )
  )
)

(defn authors->string [authors]

    (apply str (interpose ", " (map author->string authors)))

)

(defn book->string [book]
  (let [title (:title book) auths (authors->string (:authors book))]
    (str title ", written by " auths)
  )
)

(defn books->string [books]
  (let [count (count books) ]
     (cond
        (= count 0) "No books."

         (= count 1) (str count " book. " (apply str (interpose ". " (map book->string books))) ".")

         :else (str count " books. " (apply str (interpose ". " (map book->string books))) ".")

      )
  )
)

(defn books-by-author [author books]

  (filter
    (fn [book] (has-author? book author)) books
  )
)

(defn author-by-name [name authors]
  (first
    (filter
      (fn [author] (= (:name author) name)) authors
    )
  )
)

(defn living-authors [authors]
  (filter
    alive? authors
  )
)

(defn has-a-living-author? [b]
  (let [auths (:authors b)]
    (cond
      (empty? (living-authors auths)) false
      :else true
    )
  )
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
