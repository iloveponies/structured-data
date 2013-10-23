(ns structured-data)

(defn do-a-thing [x]
 (let [plus (+ x x) ]
   (Math/pow plus plus)
 )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [frst (get v 0)
        scnd (get v 2)]
    (+ frst scnd))
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
    (- y2 y1)))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2 ) (<= y1 p2 y2))
  ))

(defn contains-rectangle? [outer inner]
(let [[[x1 y1] [x2 y2]] inner]
  (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2))
       (contains-point? outer (point x1 y2)) (contains-point? outer (point x2 y1)))
  true
  false))

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (== 1 (author-count book))
    false
    true))

(defn add-author [book new-author]
 (let [au (:authors book)]
   (conj au new-author)
   (assoc book :authors (conj au new-author))
   )

)

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]

  (let [seccy (fn [x] (get x 1) ) ]
    (map seccy collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or
   (apply >= a-seq)
   (apply <= a-seq))
  )


(defn stars [n]

  (apply str (repeat n "*"))

  )

(defn toggle [a-set elem]
  )

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%



