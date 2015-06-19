(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
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
  (= (height rectangle)(width rectangle))
)

(defn area [rectangle]
  (*(height rectangle)(width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2 ) (<= y1 y3 y2) )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x3 y3][x4 y4]] inner]
  (and (contains-point? outer (point x3 y3))
       (contains-point? outer (point x4 y4)))
  )

)


(defn title-length [book]
  (count( :title book)))

(defn author-count [book]
  (count(:authors book)))

(defn multiple-authors? [book]
  (< 1 (count(:authors book)))
  )

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors))
  )

(defn alive? [author]
  (not (contains? author :death-year ))
  )

(defn element-lengths [collection]
    (let [counter (fn [x] (count x))]
      (map counter collection)))

(defn second-elements [collection]
  (let [second-picker (fn [x] (get x 1))]
    (map second-picker collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)
  (count (set a-seq)))))

(defn old-book->new-book [book]
    (assoc book :authors (set(:authors book))))

(defn has-author? [book author]
  (let [new-book (fn [x] (old-book->new-book x))]
  (contains? (:authors (new-book book)) author)))

(defn authors [books]
  (let [bookAuthors (fn [x] (:authors x))]
    (apply clojure.set/union (map bookAuthors books))
    ))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [lived (fn [author] (if (contains? author :birth-year)
                            (str " (" (:birth-year author) " - " (:death-year author) ")")))
        authorToString (fn [author] (str (:name author)
          (lived author)
         ))
        ]
    (authorToString author)
         ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by "
       (authors->string (authors [book]))))

(defn books->string [books]
  (let [bookTitles (fn [books] (apply str (interpose ". " (map book->string books))))
        bookStr (fn [books] (cond (= (count books) 0) "No books."
                                        (= (count books) 1) (str "1 book. " (bookTitles books) ".")
                                        :else (str (count books) " books. " (bookTitles books) ".")))
        ]
    (bookStr books))
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first(filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn[x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn[x] (has-a-living-author? x)) books))

; %________%
