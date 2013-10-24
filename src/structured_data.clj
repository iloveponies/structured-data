(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] 
    (+ z x)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn abs [x]
    (if (> x 0)
        x
        (- x)
    )
)

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (- x1 x2))
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (- y1 y2))
    ))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
        (and (<= x1 xp x2) (<= y1 yp y2) )
  ))

(defn contains-rectangle? [outer inner]
  (let [[p1, p2] inner]
      (and (contains-point? outer p1) (contains-point? outer p2) )
    )
  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
  (assoc book :authors new-authors)

  )
)

(defn alive? [author]
  (if (contains? author :death-year) false true)
)

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [take-second (fn [col] (get col 1))]
  (map take-second collection))
)

(defn titles [books]
  (let [take-title (fn [col] (get col :title))]
  (map take-title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (cond
    (> n 0) (str "*" (stars (- n 1)))
    :else ""
    )
)

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)

    ))

(defn asd [s]
  (let [a? (fn [x] (== x "A")) ]
    (filter a? s)
  )
)

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (frequencies a-seq)))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get (old-book->new-book book) :authors ) author ))

(defn authors [books]
  (apply clojure.set/union (map (fn [x] (get x :authors)) books)))

(defn all-author-names [books]
  (set (map (fn [x] (get x :name)) (authors books))))



(defn asd [a]
  (if (empty? (str (get {:birth-year 1} :birth-year))) "" "haa")
)


(defn author->string [author](
  let [an (fn [a] (get a :name))
       ay (fn [a] ( if (empty? (str (get a :birth-year))) "" (str " (" (get a :birth-year) " - " (get a :death-year) ")")  ))
  ]
  (str (an author) (ay author))
))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (== 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    ))

(defn books-by-author [author books]
  (
    let [t (fn [x] (has-author? x author))]
    (filter t books)
    )
  )

(defn author-by-name [name authors]
  (
    let [t (fn [x] (= name (get x :name)))]
    (first (filter t authors))
    ))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (first (living-authors (get book :authors))) true false))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
