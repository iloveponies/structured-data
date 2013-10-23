(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]

    (Math/pow xx xx))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let[[x y z] v] (+ x z) ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  (let[[[x1 y1] [x2 y2]] rectangle] (== (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (* (Math/abs (- x1 x2)) (Math/abs (- y1 y2) )) ))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (and
     (<= x1 (first point) x2)
     (<= y1 (second point) y2)
    )
  )
)





(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer]
   (let [[point1 point2] inner]
     (and (contains-point? outer point1) (contains-point? outer point2))
   )
  )
)





(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1 ))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))


(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]

  (map count collection)
  )

(defn second-elements [collection]
  (let
    [foo (fn [x] (second x))]
  (map foo collection))
)

(defn titles [books]
  (map :title books)

)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq) ))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem) ))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)) )))

(defn old-book->new-book [book]
  (assoc book :authors
    (set (book :authors))))


(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (let [foo (fn [x] (:authors x))]
  (apply clojure.set/union (map foo books))
  ))

(defn all-author-names [books]
  (let [foo (fn [x] (:name x))]
  (set (map foo (authors books) ))
       )
)

(defn author->string [author]
  (let [foo (fn [x]
       (if
         (contains? author :death-year)
         (str " (" (:birth-year author) " - "(:death-year author) ")")
         (if
           (contains? author :birth-year)
           (str " ("(:birth-year author) " - )")
           (str "")
           )
         ;(str " ")
         )
              ) ]
  (str (:name author) (foo author))))
  ;(foo author)))

(defn authors->string [authors]
  (apply str(interpose ", "(map author->string authors)))
  )

(defn book->string [book]

 (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (let
    [multi? (fn[x] (if (> x 1) (str x " books") (str x " book")) ),
    space (fn[x] (str (book->string x) (if (> (count books) 1) ". " ".")) )]

    (if
      (> (count books) 0)
      (str (apply str (multi? (count books))) ". " (apply str (map space books)))
      (str "No books.")

    )
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)

  )


(defn author-by-name [name authors]
   (first (filter (fn [x] (= (:name x) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [x]  (not(:death-year x))) authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x))  books))

; %________%
