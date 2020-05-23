(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)
  )
)

(defn spiff [v]
  (if (< (count v) 3)
      nil
      (+ (get v 0) (get v 2)
    )
   )
)

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (< (count v) 3)
      nil
      (let [[f s t] v]
        (+ f t)
      )
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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- y2 y1) (- x2 x1))
  )
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- y2 y1) (- x2 x1))
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))
  )
)

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
      (contains-point? outer p2)
    )
  )
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
   (> (count (:authors book)) 1)
)

(defn add-author [book new-author]
  (let [orig (book :authors)
        ext (conj orig new-author)]
    (assoc book :authors ext)
  )
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [my-second (fn [x] (get x 1))]
    (map my-second collection)
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
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
  (let [x (count a-seq)
        y (count (set a-seq))]
    (not (= x y))
  )
)

(defn old-book->new-book [book]
  (let [entry (set (:authors book))]
    (assoc book :authors entry)
  )
)

(defn has-author? [book author]
  (let [authors (:authors book)]
  (contains? authors author))
)

(defn authors [books]
  (let [auths (map :authors books)]
    (apply clojure.set/union auths)
  )
)

(defn all-author-names [books]
  (let [auths (authors books)]
    (set (map :name auths))
  )
)

(defn author->string [author]
  (let [name (:name author)
        by (:birth-year author)
        dy (:death-year author)]
    (str name (cond
                dy (str " (" by " - " dy ")")
                by (str " (" by " - )")
                :else nil
      )
    )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string(:authors book))]
    (str title ", written by " authors)
  )
)

(defn books->string [books]
  (let [book-list (apply str (interpose ". " (map book->string books)))
        nr-of-books (count books)
      ]
    (cond
      (= nr-of-books 0) "No books."
      (= nr-of-books 1) (str "1 book. " book-list ".")
      (> nr-of-books 1) (str nr-of-books " books. " book-list ".")
    )
  )
)

(defn books-by-author [author books]
  (let [filter-func (fn [book] (contains? (:authors book) author) )]
    (filter filter-func books)
  )
)

(defn author-by-name [name authors]
  (let [filter-func (fn [author] (= name (:name author) ))]
    (first (filter filter-func authors))
  )
)

(defn living-authors [authors]
  (let [filter-func (fn [author] (not (contains? author :death-year)) )]
    (filter filter-func authors)
  )
)

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))
  )
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
