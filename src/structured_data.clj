(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
  (Math/pow x2 x2))
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
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
  (- x2 x1))
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (- y2 y1)
  )
)

(defn square? [rectangle]
   (= (width rectangle) (height rectangle))
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [x y] point]

    (and (<= x1 x x2) (<= y1 y y2))
  )
)

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) (contains-point? outer point2))
    )
)

(defn title-length [book]
  (count (get book :title))
)

(defn author-count [book]
  (count (get book :authors))
)

(defn multiple-authors? [book]
  (< 1 (author-count book))
)

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (assoc authors (count authors) new-author))
  )
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)
  )
)

(defn titles [books]
  (let [getTitle (fn [x] (get x :title))]
    (map getTitle books)
  )
)

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors)))
)

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map (fn [x] (get x :authors)) books)))

(defn all-author-names [books]
  (set (map (fn [x] (get x :name))(authors books))))

(defn author->string [author]
  (let [author-name (get author :name)
        birth (if 
                (contains? author :birth-year) 
                (str " (" (get author :birth-year) " - ")
                "")
        death (if (contains? author :death-year)
                (str (get author :death-year))
                "")]
    (str author-name birth death (if (= birth "") "" ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-name (get book :title)
        authors (get book :authors)]
    (str book-name (if (< 0 (count authors)) 
                     (str ", written by " (authors->string authors))
                     ""))))

(defn books->string [books]
  (let [count-string (cond
                      (< 1 (count books)) (str (count books) " books. ")
                      (= 1 (count books)) (str "1 book. ")
                      (= 0 (count books)) (str "No books"))]
  (str count-string 
       (apply str (interpose ", " (map book->string books))) "."
  ))
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (get x :name) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))