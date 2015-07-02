(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x )]
  (Math/pow xx xx))
  )

(defn spiff [v]
   (let [f (get v 0)
         t (get v 2)]
     (+ f t)
   )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
   (let [[x y z] v]
     (+ x z))
  )

(defn point [x y]
  [x y]
  )

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]
  )

(defn width [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
    (if (= (height rectangle) (width rectangle)) true false)
)

(defn area [rectangle]
   (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle
          [x3 y3] point]
    (if(and (<= x1 x3 x2) (<= y1 y3 y2)) true false))
  )

(defn contains-rectangle? [outer inner]
      (let [[[x1 y1] [x2 y2]] outer
          [[x3 y3] [x4 y4]] inner]
        (if (and (<= x1 x3) (>= x2 x4) (<= y1 y3) (>= y2 y4)) true false)
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
    (= 1 (author-count book)) false
   :else true
   )
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
(map count collection)
  )

(defn second-elements [collection]
 (map second collection)
)

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply >= a-seq) true
   (apply <= a-seq) true
   :else false
   )
    )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (cond
  (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (set (apply concat (map :authors books)))
)

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years (if (contains? author :birth-year)
     (str " (" (:birth-year author) " - " (:death-year author) ")")
                nil)]
    (str (:name author) years))
    )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

 (defn book->string [book]
  (let [writer (if (contains? book :authors)
                (str ", written by " (authors->string (:authors book))
                ) nil)]
  (str (:title book) writer))
  )

(defn books->string [books]
  (let [amount (count books)

        number (cond
                (== 0 amount) "No books."
                (== 1 amount) "1 book."
                :else (str amount " books."))

        books-string (map book->string books)
        books-with-dots (interpose ". " books-string)
        books-stringed (apply str books-with-dots)
        ready-coll (if (< 0 amount) (str " " books-stringed ".") books-stringed)
        ]
     (str number ready-coll)
    )
)

(defn books-by-author [author books]
    (let [has-this-author? (fn [book-list] (has-author? book-list author))]
        (filter has-this-author? books))
  )


(defn author-by-name [name authors]
    (let [persons-name? (fn [person] (if (= (:name person) name) true false))]
        (first (filter persons-name? authors)))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
