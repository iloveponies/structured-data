(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx))
  )

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)
    )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
     (+ x z)
     )
    );

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]
  )

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))
    )
  )

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false
    )
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2) )
  ))

(defn contains-rectangle? [outer inner]
  (let [[i1 i2] inner
        [x1 y1] i1
        [x2 y2] i2
        p1 (point x1 y1)
        p2 (point x2 y2)
       ]
    (and (contains-point? outer p1) (contains-point? outer p2))
    ))

(defn title-length [book]
  (count (get book :title))
  )

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (>= (author-count book) 2)
  )

(defn add-author [book new-author]
  (let [oldauth (get book :authors)
        newauth (merge oldauth new-author)]
    (assoc book :authors newauth)
    )

    )
(defn alive? [author]
  (not (contains? author :death-year))
)
(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [collection] ( get collection 1)) collection)
)

(defn titles [books]
  (map (fn [books] ( get books :title)) books)
  )

 (defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
   (if (empty? a-seq)
     false
     (let [elem (first a-seq)
         res (rest a-seq)
         con (not (nil? (some #{elem} res )))]
          (or (contains-duplicates? (rest a-seq)) con )
       )

     )

  )

(defn old-book->new-book [book]
   (let [auth (into #{} (get book :authors))]
    (assoc book :authors auth)
    )
  )

(defn has-author? [book author]
   (not (nil? (some #{author} (get book :authors))))
  )

(defn authors [books]
  (let [author-names
         (fn [book] (get book :authors))]
    (set (apply concat (map author-names books))))
  )

(defn all-author-names [books]
    (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))


(defn author->string [author]
  (let [nam (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)
        bdstr (apply str (interpose " - " [birth, death] ))]
    (if (nil? birth)
        nam
        (apply str nam " ("bdstr ")" )
    )))

(defn authors->string [authors]
  (apply str (interpose ", "(map (fn [authors] ( author->string authors)) authors)))
  )
(defn book->string [book]
  (let [title (get book :title)
        authors (authors->string (get book :authors))
        ]
    (apply str title ", written by " authors))

  )

(defn books->string [books]
  (if (empty? books )
    (apply str "No books.")
    (let [count (count books)
          book (apply str (interpose ", "(map (fn [books] ( book->string books)) books)))]
      (if (> count 1)
        (apply str count " books. " book ".")
        (apply str count " book. " book ".")
      )

    )
 ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
   (first (filter (fn [author] (= (:name author) name)) authors))
  )

(defn living-authors [authors]
 (filter (fn [x] (true? (alive? x))) authors)
)

(defn has-a-living-author? [book]
  (let [authors (get book :authors)]
    (not (empty? (living-authors authors)))
    )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________% °~° ^____^
