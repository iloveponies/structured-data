(ns structured-data)

(defn do-a-thing [x]
  (let [result (+ x x)]
    (Math/pow result result)
  ))


(defn spiff [v]
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[first _ third] v]
    (+ first third)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
)


(defn square? [rectangle]
    (= (width rectangle) (height rectangle))
)


(defn area [rectangle]
    (* (width rectangle) (height rectangle))
)


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))


(defn contains-rectangle? [outer inner]
  (let [[inner-top-left inner-bottom-right] inner]
    (and (contains-point? outer inner-top-left)
         (contains-point? outer inner-bottom-right))
    )
)

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)
  ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [s] (first (rest s)))]
    (map second-element collection))
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
    (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
        years (if (author :birth-year) (str " ("
                                    (:birth-year author)
                                    " - "
                                    (:death-year author)
                                    ")"))]
    (str name years))

)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) " written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (pos? (count books))
    (str (count books)
         (if (= (count books) 1) " book" " books")
         ". "
         (apply str (interpose ". " (map book->string books)))
         ".")
    "No books."))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

;=> (little-schemer cities embassytown silmarillion)

; %________%
