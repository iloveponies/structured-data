(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [px py] point]
    (and
     (or (<= x1 px x2) (<= x2 px x1))
     (or (<= y1 py y2) (<= y2 py y1))
    )
  ))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1)
         (contains-point? outer point2)
    )
  )
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author[book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)
  )
)

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [e (fn [li] (get li 1))]
    (map e collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
)

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)
  )
)

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [auths (map :authors books)]
    (apply clojure.set/union auths)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [n (apply str [(:name author) " "])
        y (apply str ["(" (:birth-year author) " - " (:death-year author) ")"])
       ]
    (if (:birth-year author)
      (apply str [n y])
      (str (:name author))
    )
  )
)


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str [(:title book) ", written by " (authors->string (:authors book)) ])
)

(defn books->string [books]
  (cond (empty? books) "No books."
        (= (count books) 1) (apply str ["1 book. " (str (book->string (get books 0))) "."])
        :else (apply str (apply str [(count books) " books. " (apply str (interpose ". " (map book->string books))) "."]))
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
  (let [aa (first authors)]
    (cond
     (= (seq authors) nil)  nil
     (= name (:name aa)) aa
     :else (author-by-name name (rest authors))
    )
  )
)

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
)

(defn has-a-living-author? [book]
  (< 0 (count
        (living-authors (:authors book))
       )
  )
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
