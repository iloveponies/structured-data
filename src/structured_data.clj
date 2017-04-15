(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
     (Math/pow y y)
    )
  )

(defn spiff [v]
  (+ (get v 0)
  (get v 2)
  )
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
    ))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
        (if (= (- x2 x1) (- y2 y1)) true false)
    ))


(defn area [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
       (* (- x2 x1) (- y2 y1))
    ))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (let [[x3 y3] point]

        (if (and(<= x1 x3 x2) (<= y1 y3 y2))true false)
  )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[x3 y3] [x4 y4]] inner]
      (if (and (contains-point? (rectangle [x1 y1] [x2 y2])(point x3 y3))
              (contains-point? (rectangle [x1 y1] [x2 y2])(point x4 y4))
              )

      true false)

      )
    )
  )

(defn title-length [book]
  (count (get book :title))
  )

(defn author-count [book]
   (count (get book :authors))
  )

(defn multiple-authors? [book]
  (if (= 1 (author-count book))false true)

  )

(defn add-author [book new-author]
  (let [authors (get book :authors)]

    (assoc book :authors  (conj authors new-author))
   ; book
    )
  )

(defn alive? [author]
  (if (contains? author :death-year) false true)
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [munge (fn [collection] (get collection 1))]
    (map munge collection)
        )
  )

(defn titles [books]
   (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)
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

 (if(= (count a-seq) (count (set a-seq)))false true)

  )

(defn old-book->new-book [book]
  (assoc book :authors  (set (get book :authors)))
  )

(defn has-author? [book author]
  (let[authors (get book :authors)]
   (contains? authors author)
  )
  )

(defn authors [books]

  (let [author-names
         (fn [book] (get book :authors))]
 (set (apply clojure.set/union (map author-names books)))
  )
  )

(defn all-author-names [books]
 (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [nimi (get author :name)
   bhyear (get author :birth-year)
      dyear (get author :death-year)
        years (str " ("bhyear " - " dyear ")" )]

    (str nimi (if (= " ( - )" years) "" years)
    )
     )
  )






(defn authors->string [authors]
(let [author-names
         (fn [author](author->string author))]
     (apply str(interpose ", " (map author-names authors))))

 )


(defn book->string [book]
 (let [nimi (get book :title)
       kirjoittaja (authors->string (get book :authors))]
   (str nimi ", written by " kirjoittaja)

   )
  )

(defn books->string [books]
 (let [kirjat
       (fn [book](book->string book))
       kirjat_tuloste (apply str(interpose ". " (map kirjat books)))
       ]

   ;(map kirjat books) (count books) " books."
  (cond
   (= 0 (count books)) "No books."
   (= 1 (count books)) (str "1 book. " (book->string (get books 0))".")
    :else         (str (count books) " books. " kirjat_tuloste "."
 ))
)
 )

;(book->string (get books 0)  )

   ;  )


(defn books-by-author [author books]

  (filter (fn [book] (has-author? book author))books)
  ;(filter (has-author? book author)  books)
  )

(defn author-by-name [name authors]
  (let [tutkittava
    (first (filter (fn [author] (= name ( get author :name))) authors))]
    (if (= tutkittava ()) nil tutkittava)
  )
      )



(defn living-authors [authors]
  (let [tutkittava
    (filter (fn [author] (alive? author)) authors)]
    tutkittava
  )
  )

(defn has-a-living-author? [book]
    (if(empty? (living-authors (get book :authors)))false true)

  )

(defn books-by-living-authors [books]
  (let [tutkittava
    (filter (fn [book] (has-a-living-author? book)) books)]
    tutkittava
  )
  )

; %________%

