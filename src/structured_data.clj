(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle))
    true
    false
    )
  )

  (defn area [rectangle]
  (cond
    (< (height rectangle) 0) 0
    (< (width rectangle) 0) 0
    :else (* (width rectangle) (height rectangle))
    ))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle]
    (let [[xc yc] point]
      (if (and
            (<= x1 xc x2)
            (<= y1 yc y2)
            )
        true
        false
        ))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (if (and
          (contains-point? outer point1)
          (contains-point? outer point2)
        )
      true
      false
      )))

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false
  ))

(defn add-author [book new-author]
   (assoc book :authors (conj (:authors book) new-author))
)

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
    (let [second-element (fn [collec] (get collec 1))]
      (map second-element collection)
      )
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (not (contains? a-set elem))
    (conj (set a-set) elem)
    (disj (set a-set) elem)
  ))

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq))
      true
      false
    ))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false
    )
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (let [author-names 
	(fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (if (contains? author :death-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (if (contains? author :birth-year)
      (str (:name author) " (" (:birth-year author) " - )")
      (str (:name author))
      )
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    (str "No books.")
    (if (== (count books) 1)
      (str "1 book. "(apply str (interpose ", " (map book->string books))) ".")
      (str (count books) " books. "(apply str (interpose ", " (map book->string books))) ".")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (if (.contains (map :name authors) name)
    (first (filter (fn [x] (= name (:name x))) authors))
    nil
    )
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
