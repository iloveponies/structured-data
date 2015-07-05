(ns structured-data)

(defn do-a-thing [x]
  (let [f1 (+ x x)]
    (Math/pow f1 f1)
   ))

(defn spiff [v]
  (+
     (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
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
    (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
   ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [
        [[x1 y1] [x2 y2]] rectangle
        [x3 y3] point
        ]
    (and (<= x1 x3 x2) (<= y1 y3 y2))
    ))

(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner]
    (and
     (contains-point? outer p1) (contains-point? outer p2)
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
  (> (count (:authors book)) 1)
  )

(defn add-author [book new-author]
  (let [arr (:authors book)
        ]
    (assoc book :authors (conj arr new-author))
    )
  )

(defn alive? [author]
  (not (boolean (:death-year author)))
  )

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec-elem (fn [x] (second x))]
    (map sec-elem collection)
    ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  ( or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (let [tmp (count (set a-seq))]
    (not= tmp (count a-seq))
    )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)) )


(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [birthDate (:birth-year author)
        deathYear (:death-year author)
        authorName (:name author)
        ]

        (if (or (boolean deathYear) (boolean birthDate))
          (str authorName " (" birthDate " - " deathYear ")")
          (str authorName)
          )
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [cnt (count books)
        bookStr (apply str (interpose ", "  (map book->string books)))
        ]
      (cond
          (= 0 cnt) (str "No books.")
          (= 1 cnt) (str "1 book. " bookStr ".")
          (> cnt 1) (str cnt " books. " bookStr ".")
          )
      )
    )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
    (first
        (filter (fn [x] (= name (:name x))) authors))
)

(defn living-authors [authors]
    (filter
        (fn [x] (alive? x))
        authors)
    )

(defn has-a-living-author? [book]
    (not (empty? (living-authors (:authors book))))
    )

(defn books-by-living-authors [books]
  (filter
      (fn [x] (has-a-living-author? x))
      books
      ))

; %________%
