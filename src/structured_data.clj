(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x )]

  (Math/pow xx xx))
  )

(defn spiff [v]
   (+ (get v 1) (get v 3))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
   (let [[x y z] v]
     (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
    (if (= (height rectangle) (width rectangle)) true false)
)

(defn area [rectangle]
   (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle
          [x3 y3] point]
    (if(and (<= x1 x3 x2) (<= y1 y3 y2)) true false))
  )

(defn contains-rectangle? [outer inner]
      (let [[[x1 y1] [x2 y2]] outer
          [[x3 y3] [x4 y4]] inner]
        (if (and (<= x1 x3) (>= x2 x4) (<= y1 y3) (>= y2 y4)) true false)
        )

  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (cond
    (= 1 (author-count book)) false
   :else true
   ))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]

  )

(defn second-elements [collection]
 (map second collection)
)

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply >= a-seq) true
   (apply <= a-seq) true
   :else false
   )
    )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (cond
  (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
)

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (set (apply concat (map :authors books)))
)

(defn all-author-names [books]
  (set (apply concat (map :authors books))))

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
