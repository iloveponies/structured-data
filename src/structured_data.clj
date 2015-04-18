(ns structured-data)


(defn do-a-thing [x]
  (let [twice (+ x x)]
   (Math/pow twice twice)
  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
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
  (let [[[x1 y1] [x2 y2]] rectangle]
     (== (- x2 x1) (- y2 y1))
   )
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1) )
   )
 )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point
        ]
    (and (<= x1 x3 x2) (<= y1 y3 y2))
    )
   )

(defn contains-rectangle? [outer inner]
   (let [
        [[x1 y1] [x2 y2]] inner
        ]
    (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
   (count (:authors book))
  )

(defn multiple-authors? [book]
  (<= 2 (count (:authors book))
      )
  )

(defn add-author [book new-author]
  (let [
      kirjoittajat (:authors book)
      kaikki  (conj kirjoittajat new-author)
      ]
    (assoc book :authors kaikki)
  )
)

(defn alive? [author]
(if(contains? author :death-year)
  false
  true
  )
)

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [toka (fn [x] (get x 1))]
    (map toka collection)
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
   (if (not= (count a-seq) (count (set a-seq)))
     true
     false
     )
  )

(defn old-book->new-book [book]
  (let [kirjoittajat (set(:authors book))]
  (assoc book :authors kirjoittajat)
    )
)

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false
  )
)
(defn authors [books]
 (apply clojure.set/union (set (map :authors books)))
  )

(defn all-author-names [books]
 (set(map :name (authors books)))
  )

(defn author->string [author]
       (let [vuodet
             (str " (" (:birth-year author) " - " (:death-year author) ")")]
         (if (contains? author :birth-year)
           (str (:name author) vuodet)
           (str (:name author))
         )
    )
  )

(defn authors->string [authors]
(apply str (interpose ", "(map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
  (== (count books) 0) (str "No books.")
  (== (count books) 1) (str (apply str "1 book. "(map book->string books))".")
   :else (str (apply str (count books) " books. " (interpose ". " (map book->string books)) )".")
  )
)

 (defn books-by-author [author books]
  (filter (fn [book](has-author? book author)) books)
  )

(defn author-by-name [name authors]
(first (filter (fn[author] (= name(:name author))) authors))
  )

(defn living-authors [authors]
  (filter (fn[author] (alive? author)) authors)
  )

(defn has-a-living-author? [book]
(if(empty? (filter(fn[author] (alive? author)) (:authors book)))
  false
  true
  )
  )

(defn books-by-living-authors [books]
  (filter (fn [book](has-a-living-author? book)) books)
  )

; %________%
