(ns structured-data)

(defn do-a-thing [x]
  (let [xd (+ x x)]
    (Math/pow xd xd)
  )
)

(defn spiff [v]
  (if (< (count v) 3)
    "?"
    (+ (get v 0) (get v 2))
  )
)

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (< (count v) 3)
    "?"
    (let [[a b c] v]
      (+ a c)
    )
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn rTop [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    y2))

(defn rRight [[[x1 y1] [x2 y2]]]
  x2)

(defn rBottom [[[x1 y1] [x2 y2]]]
  y1)

(defn rLeft [[[x1 y1] [x2 y2]]]
  x1)

(defn contains-point? [rectangle point]
  (let [[x y] point]
    (and 
      (<= (rBottom rectangle) y (rTop rectangle))
      (<= (rLeft rectangle) x (rRight rectangle))
    )
  )
)

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and 
      (contains-point? outer bottom-left)
      (contains-point? outer top-right)
    )
  )
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collections]
  (let [pickSecond (fn [collection] (get collection 1))]
    (map pickSecond collections)))

(defn titles [books]
  (let [pickTitle (fn [b] (:title b))]
    (map pickTitle books)))

(defn monotonic? [a-seq]
  (or 
    (apply <= a-seq)
    (apply >= a-seq)
  ))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  ))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat(map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str 
    (:name author) 
    (if (contains? author :birth-year)
      (str 
        " (" 
        (:birth-year author)
        " - "
        (if (contains? author :death-year)
          (str (:death-year author) ")")
          ")"
        )
      )
    )
  )
)

(defn authors->string [authors]
  (cond
    (zero? (count authors))
      ""
    (== (count authors) 1)
      (author->string (first authors))
    :else
      (apply str (interpose ", " (map author->string authors)))
  )
)

  

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookCount (count books)]
    (cond
      (zero? bookCount)
        "No books."
      (== bookCount 1)
        (str "1 book. " (book->string (first books)) ".")
      :else
        (str bookCount " books. " (apply str (interpose ". " (map book->string books))) ".")
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (let [found (filter (fn [a] (= (:name a) name)) authors)]
    (if (zero? (count found))
      nil
      (first found)
    )
  )
)

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
