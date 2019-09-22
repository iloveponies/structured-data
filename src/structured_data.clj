(ns structured-data)

(defn do-a-thing [x]

  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
  )

(defn spiff [v]
  (if ( <= 3 (count v))
  (+ (get v 0) (get v 2))
  "?"
  )
)

(defn cutify [v]

  (conj v "<3")

  )


(defn spiff-destructuring [v]

  (if ( <= 3 (count v))
  (let [[x y z] v]
    (+ x z))
  "?"
  )

  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]

  (

   let

   [[ [x1 y1]  [x2 y2]] rectangle]
     (- x2 x1)
   )


  )

(defn height [rectangle]

  (let [[[x1 y1] [x2 y2]] rectangle]
      (- y2 y1)
   )

  )



(defn square? [rectangle]

  ( == (width rectangle) (height rectangle))

  )


(defn area [rectangle]

  (* (width rectangle) (height rectangle))

  )

(defn contains-point? [rectangle point]

  (let

    [[[x1 y1] [x2 y2]] rectangle
     [x3 y3] point
     ]

    (and (and (>= x3 x1) (<= x3 x2))
         (and (>= y3 y1) (<= y3 y2))
         )

    )

  )

(defn contains-rectangle? [outer inner]

    (let

      [[p1 p2] inner]

      (and (contains-point? outer p1) (contains-point? outer p2) )

      )

  )


(defn title-length [book]

  (count (:title book))

  )


(defn author-count [book]

  (count (:authors book))

  )

(defn multiple-authors? [book]

  (< 1 (author-count book))

  )




(defn add-author [book new-author]

  (

       let
       [

        authors (conj (:authors book) new-author)]


       (assoc book :authors authors)

   )

  )


(defn alive? [author]

  (not (contains? author :death-year))

  )


(defn element-lengths [collection]

  (map count collection)

  )


(defn second-elements [collection]

  (map (fn [col] (get col 1)) collection)

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
   (< (count (set a-seq)) (count a-seq))
  )

(defn old-book->new-book [book]

  (let
    [authors (:authors book)]
    (assoc book :authors (set authors))
    )

  )



(defn has-author? [book author]

  (contains? (:authors book) author)

  )




(defn authors [books]



  (

   apply clojure.set/union
   (map (fn [book] (:authors book)) books)


   )


  )
    ;=> #{china, friedman, felleisen}



(defn all-author-names [books]

  (

   set (map (fn [author] (:name author)) (authors books) )

   )

  )



(defn author->string [author]


  (cond
     (and (contains? author :birth-year)  (contains? author :death-year)) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
     (contains? author :birth-year) (str (:name author) " (" (:birth-year author)  " - )")
     :else
     (str (:name author))
   )
)


(defn authors->string [authors]

  (apply str (interpose ", " (map author->string authors)))

  )


(defn book->string [book]

  (str (:title book) ", written by " (authors->string (:authors book)))

  )



(defn books->string [books]

  (let

    [found (map book->string books)
     cnt (count found)
     ]

    (cond (== 0 cnt) "No books."
        (== 1 cnt) (str "1 book. " (apply str (interpose ". " found)) ".")
        (< 1 cnt) (str  cnt " books. " (apply str (interpose ". " found)) ".")
      )


    )

)


(defn books-by-author [author books]

  (filter (fn [book] (contains? (:authors book) author)) books)

  )


(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)
  )
)



(defn living-authors [authors]

  (filter alive? authors)

  )


(defn has-a-living-author? [book]

  (not (empty? (living-authors (:authors book))))

  )


(defn books-by-living-authors [books]

  (filter has-a-living-author? books)

  )



; %________%
