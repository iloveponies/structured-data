(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)) )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    do
    (Math/abs(- x1 x2))
    ))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (Math/abs(- y1 y2))
    ))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle]
    (let [[xp yp] point]
      (and (<= x1 xp x2) (<= y1 yp y2))
    )
  )
)

(defn contains-rectangle? [outer inner]
  (let [[i1 i2] inner]
    (and (contains-point? outer i1) (contains-point? outer i2))
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
  (assoc book :authors (conj (:authors book) new-author))
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [fun (fn [v] (get v 1))]
    (map fun collection)
    )
  )

(defn titles [books]
  (let [fun (fn [v] (:title v))]
    (map fun books)
    ))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
     death (str name " (" (str birth) " - " (str death) ")")
     birth (str name " (" (str birth) " - )")
     :else (str name)
     )
  ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [c (count books)]
    (cond
     (== c 0) (str "No books.")
     (== c 1) (str "1 book. " (book->string (first books)) "." )
     :else (str (apply str (interpose ". " (cons (str c " books") (seq (map book->string books))))) ".")
    )
  )
  )

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (:name a))) authors))
  )

(defn living-authors [authors]
  (filter (fn [a] (alive? a)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))

; %________%
