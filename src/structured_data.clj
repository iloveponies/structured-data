(ns structured-data)

(defn do-a-thing [x]
    (let [xx (+ x x)]
    (Math/pow (+ x x) (+ x x))
    )
)

(defn spiff [v]
  (+(get v 0)(get v 2))
)

(defn cutify [v]
  (conj v "<3")

)

(defn spiff-destructuring [v]
  (let [[x b c] v]
  (+ x c))
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
    (if (= 0 (- (- y2 y1) (- x2 x1))) true false)
  )
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1) )
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
   (if (and (<= x1 x3 x2) (<= y1 y3 y2))true false)
  )

)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
   (if (and (contains-point? (rectangle [x1 y1] [x2 y2]) (point x3 y3))
            (contains-point? (rectangle [x1 y1] [x2 y2]) (point x4 y4))
      )true false)
  )
)

(defn title-length [book]
  (count(:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (if(< 1 (author-count book)) true false)
)

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author))
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [munge (fn [x] (get x 1))]
    (map munge collection))
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (not (=(count (set a-seq)) (count a-seq)))
)

(defn old-book->new-book [book]
  (assoc book :authors (get book :authors))
)

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
