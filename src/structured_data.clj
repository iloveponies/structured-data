(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x )]

  (Math/pow xx xx))
  )

(defn spiff [v]
  (let [a (get v 1)
        b (get v 3)]
    (+ a b)
  )
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
  :-)

(defn height [rectangle]
  :-)

(defn square? [rectangle]
  :-)

(defn area [rectangle]
  :-)

(defn contains-point? [rectangle point]
  :-)

(defn contains-rectangle? [outer inner]
  :-)

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
