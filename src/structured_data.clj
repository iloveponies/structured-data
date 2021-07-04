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
  (let [[a b c] v]
    (+ a c)
    )

  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[xa ya] [xb yb]] rectangle]
    (- xb xa)
  )
)

(defn height [rectangle]
  (let [[[xa ya] [xb yb]] rectangle]
    (- yb ya)
  )
)

(defn square? [rectangle]
  (== (width rectangle)(height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[xa ya] [xb yb]] rectangle]
    (let [[xp yp] point]
      (and (<= xa xp xb) (<= ya yp yb))
      )
    )
  )



(defn contains-rectangle? [outer inner]
  (let [[ap bp] inner]
      (and
       (contains-point? outer ap)
       (contains-point? outer bp)
      )
    )
)




(defn title-length [book]
  (count (:title book))
  )
(defn author-count [book]
  (count (:authors book)
  )
)

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )


(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )


(defn second-elements [collection]
  (let [getsecond (fn [v] (get v 1))]
    (map getsecond collection)
    )
  )


(defn titles [books]
  (map :title books )
  )

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq))
)



(defn stars [n]
  (apply str (repeat n "*"))
  )


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not
    (==
     (count a-seq)
     (count (set a-seq))
    )
  )
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
  (set(map :name (authors books)))
  )


(defn author->string [author]
  (if (contains? author :birth-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (str (:name author))
    )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
    (== (count books) 0)"No books."
    (== (count books) 1)(str (count books)" book. " (apply str (interpose ". " (map book->string books))) ".")
    :else (str (count books)" books. " (apply str (interpose ". " (map book->string books)))".")
   )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )




(defn author-by-name [name authors]
  (defn compare-names [author]
    (= name (:name author))
    )
  (first(filter compare-names authors))
)

(defn living-authors [authors]
  (filter alive? authors)
  )


(defn has-a-living-author? [book]
  (not(empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%