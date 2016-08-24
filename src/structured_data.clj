(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
)

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b))
)

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c))
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
  (let [h (height rectangle)
        w (width rectangle)]
    (= w h)
  )
)

(defn area [rectangle]
    (let [h (height rectangle)
        w (width rectangle)]
    (* w h)
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]

    (and (<= x1 px x2) (<= y1 py y2))
  )
)

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]

    (and (contains-point? outer point1) (contains-point? outer point2))
  )
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [curr-authors (:authors book)]
    (assoc book :authors (conj curr-authors new-author)))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsh (fn [coll] (get coll 1))]
    (map getsh collection)
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
  (let [currsize (count a-seq)]
    (not= (count (set a-seq)) currsize)
    )
  )

(defn old-book->new-book [book]
  (assoc book :authors (into #{} (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn authors [books]
  (set (apply concat (map #(:authors %) books))))

(defn author->string [author]
  (let [name (:name author)
        death-year (:death-year author)
        birth-year (:birth-year author)]
    (if (nil? birth-year)
      name
      (str name " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (case (count books)
      1 (str "1 book. " (book->string (first books)) ".")
      (str (count books) " books. " (apply str (interpose ", " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler" :birth-year 1947 :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})

;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title "The Little Schemer" :authors #{friedman, felleisen}})

;(def books [cities, wild-seed, embassytown, little-schemer])