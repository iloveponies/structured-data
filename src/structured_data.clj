(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    )
)

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)
    )
)

(defn cutify [v]
  (conj v "<3"
  )
)

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[a b] [c d]] rectangle]
    (- c a)
    )
)

(defn height [rectangle]
  (let [[[a b] [c d]] rectangle]
    (- d b)
    )
)

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle)) true false)
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (cond
     (and (>= x2 x x1) (>= y2 y y1)) true
     :else false
     )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[a b] inner]
    (cond
     (and (contains-point? outer a)
          (contains-point? outer b)) true
     :else false
    )
  )
)

(defn title-length [book]
  (count
   (:title book)
   )
  )

(defn author-count [book]
  (count
   (:authors book)
   )
  )

(defn multiple-authors? [book]
  (if (>= (count (:authors book)) 2) true false)
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)
    )
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (
                 fn [x] (get x 1))]
    (map second collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false
  )
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)
  )
  )

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq)) true false)
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))
    )
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))
  ))

(defn author->string [author]
  (let [nimi
        (:name author)
        birth
        (:birth-year author)
        death
        (:death-year author)]
    (cond
    (contains? author :death-year) (apply str [nimi " " "(" birth " - " death ")"])
    (contains? author :birth-year) (apply str [nimi " " "(" birth " - " ")"])
    :else (str nimi)
    )
  )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (set (map author->string authors)
  ))))

(defn book->string [book]
  (let [writers (authors->string (:authors book))
        title (:title book)]
  (str title ", written by " writers
         )
    )
  )

(defn books->string [books]
  (cond
   (empty? books) "No books."
   (== (count books) 1) (str "1 book. " (apply str (map book->string books)) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) "."))
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not(empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
