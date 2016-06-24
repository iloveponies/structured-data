(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x))
  )

(defn spiff [v]
  (let [v1 (get v 0)
        v3 (get v 2)]
    (+ v1 v3))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[v1 _ v3] v]
    (+ v1 v3))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1][x2 y2]]]
  (- x2 x1)
  )

(defn height [[[x1 y1][x2 y2]]]
  (- y2 y1)
  )

(defn square? [rectangle]
  (=
    (width rectangle)
    (height rectangle)
    )
  )

(defn area [rectangle]
  (*
    (width rectangle)
    (height rectangle)
    )
  )

(defn contains-point? [[[x1 y1][x2 y2]] [x y]]
  (and
    (<= x1 x x2)
    (<= y1 y y2)
    )
  )

(defn contains-rectangle? [outer [point1 point2]]
  (and
    (contains-point? outer point1)
    (contains-point? outer point2)
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
  (let [old-authors (:authors book)]
    (assoc book :authors (conj old-authors new-author))
    )
  )

(defn alive? [author]
  (not
    (contains? author :death-year)
    )
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (map
    (fn [coll] (get coll 1))
    collection
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
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
  (not
    (=
      (count a-seq)
      (count (set a-seq))
      )
    )
  )

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))
    )
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union
    (map :authors books)
    )
  )

(defn all-author-names [books]
  (set
    (map :name (authors books))
    )
  )

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        years (if (= birth-year nil)
                ""
                (str
                  " ("
                  birth-year
                  " - "
                  (if (= death-year nil)
                    ""
                    death-year
                    )
                  ")"
                  )
                )
        ]
      (str name years)
    )
  )

(defn authors->string [authors]
  (apply str
    (interpose
      ", "
      (map author->string authors)
      )
    )
  )

(defn book->string [book]
  (str
    (:title book)
    ", written by "
    (authors->string (:authors book))
    )
  )

(defn books->string [books]
  (let [
    c (count books)
    header (cond
             (= c 0) "No books"
             (= c 1) "1 book. "
             :else (str c " books. ")
             )
    body (apply str
           (interpose ". " (map book->string books))
           )
    ]
    (str header body ".")
    )
  )

(defn books-by-author [author books]
  (filter
    (fn [book] (has-author? book author))
    books
    )
  )

(defn author-by-name [name authors]
  (first
    (filter
      (fn [author] (= (:name author) name))
      authors
      )
    )
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not
    (empty? (filter alive? (:authors book)))
    )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
