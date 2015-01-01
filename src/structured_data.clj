(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    )
  )

(defn spiff [v]
  (+ (get v 0), (get v 2)
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
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (- y2 y1))
  )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (= (- y2 y1) (- x2 x1))
    )
  )

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (* (- y2 y1) (- x2 x1))
    )
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
    do (and (<= x1 x x2) (<= y1 y y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[a1 b1] [a2 b2]] inner]
    do (and (<= x1 a1 x2) (<= y1 b1 y2) (<= x1 a2 x2) (<= y1 b2 y2))
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (count (:authors book)))
  )

(defn add-author [book new-author]
  (update-in book [:authors] conj new-author)
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count (map seq collection)))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection))
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
  (cond (contains? a-set elem) (disj a-set elem) :else (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq))
  )

(defn old-book->new-book [book]
  (update-in book [:authors] set))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
      (and birth death)
        (str name " (" birth " - " death ")")
      birth
        (str name " (" birth " - )")
      :else
        name
      )
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (= (count books) 1) (str "1 book. " (book->string (first books)) ".")
    (> (count books) 1) (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [alives (filter alive? (:authors book))]
    (> (count alives) 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
