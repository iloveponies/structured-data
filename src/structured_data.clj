(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
  (Math/pow double double)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)
    ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]

    (+ a c)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        height (- y2 y1)
        width (- x2 x1)
        ]
    width
    )
  )

(defn height [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle
        height (- y2 y1)
        width (- x2 x1)
        ]
    height
    )
  )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        sideX (- x2 x1)
        sideY (- y2 y1)

        ]

    (if (= sideX sideY) true false)
    ))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [
        [[rectX1 rectY1] [rectX2 rectY2]] rectangle
        [checkX1 checkY1] point

        ]

    (cond
     (and (<= rectX1 checkX1 rectX2)
          (<= rectY1 checkY1 rectY2)
          ) true
     :else false

     )


    ))

(defn contains-rectangle? [outer inner]
  (let
    [
     [[outerX1 outerY1] [outerX2 outerY2]] outer
     [[innerX1 innerY1] [innerX2 innerY2]] inner
    ]

    (cond

     (and (contains-point? outer [innerX1 innerY1]) (contains-point? outer [innerX2 innerY2])) true
     :else false

     )


    ))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))

(defn add-author [book new-author]

  (let [
        existingAuthors (:authors book)
        ]


  (assoc book :authors (conj existingAuthors new-author))
        )

  )

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map (fn [element] (count element)) collection))

(defn second-elements [collection]
  (let
    [
     getSecond (fn [element] (get element 1))
     ]

    (map getSecond collection)

    ))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or
       (apply <= a-seq)
       (apply >= a-seq)
       ) true
    false
    )
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
    (conj a-set elem)

    ))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false
    true))

(defn old-book->new-book [book]
  (let
    [
     authorVector (:authors book)
     authorSet (set (:authors book))
     ]
    (assoc book :authors authorSet)
    ))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true
    false))

(defn authors [books]
  (let
    [
     bookAuthors (fn [book] (:authors book))
     ]

    (set (apply concat (map bookAuthors books)))

    ))


(defn all-author-names [books]

  (set (map :name (authors books)))

  )

(defn author->string [author]

  (cond
   (:death-year author) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
   (:birth-year author) (str (:name author) " (" (:birth-year author) " - )")
   :else (str (:name author))

   )
  )

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (= 0 (count books)) "No books."
   (= 1 (count books)) (apply str "1 book. " (concat (map book->string books) "."))
   :else (apply str (count books) " books. " (concat (interpose ". " (map book->string books)) "."))

   ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author](= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (filter alive? (:authors book))) false
    true
    ))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
