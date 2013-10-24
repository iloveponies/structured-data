(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
  )

(defn spiff [v]
  (+ (get v 2) (get v 0))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))

  )

(defn height [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))

  )

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false)
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
     [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2))
      true
      false)
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[px1 py1] [px2 py2]] inner]
    (if (and (contains-point? outer [px1 py1]) (contains-point? outer [px2 py2]))
      true
      false
    )
  )
  )

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count  (get book :authors))
)

(defn multiple-authors? [book]
  (if(= (author-count book) 1)
    false
    true)
)
(defn add-author [book new-author]
  (assoc book :authors  
    (conj (:authors book) new-author))
)

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true)
  )

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [seconds (fn [x] (get x 1))]
    (map seconds collection)
    ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (apply <= a-seq) true 
    (if (apply >= a-seq) true
      false))
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
    (if (= (count (set a-seq)) (count a-seq))
      false
      true
      )
    )


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


