(ns structured-data)


(defn do-a-thing [x]
  (let [xx (+ x x)
        ]
        (Math/pow xx xx)
  )

)


(defn spiff [v]
  (+ (get v 0) (get v 2))


  )

(defn cutify [v]
  (conj v "<3")

   )

(defn spiff-destructuring [[x1 x2 x3]]
  (+ x1 x3))

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

  (if (= (height rectangle) (width rectangle)) true false)


)

(defn area [rectangle]

  (* (height rectangle) (width rectangle))



)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]

    (cond
        (and (<= x1 px x2) (<= y1 py y2)) true
        :else false




    )

  )
  )

)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (let [[[ox1 oy1] [ox2 oy2]] outer]

    (cond

     (and (contains-point? (rectangle [ox1 oy1] [ox2 oy2]) (point x1 y1)) (contains-point? (rectangle [ox1 oy1] [ox2 oy2]) (point x2 y2))      ) true
     :else false

    )
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

(if (< 1 (author-count book)) true false)

)

(defn add-author [book new-author]

  (let [new (conj (:authors book) new-author)]
    (assoc book :authors new)


   )

  )

(defn alive? [author]
(if (contains? author :death-year) false true)
  )

(defn element-lengths [collection]
 (map count collection))

(defn second-elements [collection]
 (let [element (fn [array]
                 (get array 1))]
   (map element collection))

  )

(defn titles [books]
 (map :title books)
  )

(defn monotonic? [a-seq]
(or (apply >= a-seq) (apply <= a-seq))
)

(defn stars [n]
 (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem)
   (disj a-set elem)
   :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [count-orig (count a-seq)
       count-unique (count (set a-seq))]
    (not (= count-orig count-unique))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
 (let [name-string (:name author)
       birth-year (:birth-year author)
       death-year (:death-year author)
       year-string (cond
                      (contains? author :death-year) (str " " \( birth-year " "  \- " " death-year \))
                      (contains? author :birth-year) (str " " \( birth-year " " \-  " " \))
                      :else "")]
   (str name-string year-string)))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [authors-string (authors->string (:authors book))
        title-string (:title book)]
    (apply str (interpose ", written by " [title-string authors-string]))))

(defn books->string [books]
 (let [book-count (count books)
        book-count-string (cond
                             (= 0 book-count) "No books."
                             (= 1 book-count) "1 book. "
                             :else (str book-count " books. "))
        books-string (apply str (interpose ". " (map book->string books)))
        trailing-dot-string (if (> book-count 0) "." "")]
    (str book-count-string books-string trailing-dot-string)))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
 (let [matching (filter (fn [author] (= (:name author) name)) authors)
        matches? (> (count matching) 0)]
    (if matches? (first matching) nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
 (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
