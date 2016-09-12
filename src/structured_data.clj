(ns structured-data)

(defn do-a-thing [x]
  (let [xplusx (+ x x)]
    (Math/pow xplusx xplusx)
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
(= (height rectangle) (width rectangle))
)

(defn area [rectangle]
(* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
(let [[[x1 y1][x2 y2]] rectangle [x3 y3] point]
  (and (<= x1 x3 x2) (<= y1 y3 y2))
  )
)

(defn contains-rectangle? [outer inner]
(let [[p1 p2] inner]
  (and (contains-point? outer p1) (contains-point? outer p2))
  )
)

(defn title-length [book]
(count (:title book))
  )

(defn author-count [book]
(count (:authors book)
  )
)

(defn multiple-authors? [book]
(<=  2 (author-count book)
  )
)

(defn add-author [book new-author]
(assoc book :authors (conj (:authors book) new-author
  ))
)

(defn alive? [author]
(not (contains? author :death-year))
)

(defn element-lengths [collection]
(map count collection
  )
)

(defn second-elements [collection]
(let [funct (fn [x] (get x 1))]
  (map funct collection))
)

(defn titles [books]
(map :title books
  )
)

(defn monotonic? [a-seq]
(or  (apply >= a-seq)
(apply <= a-seq)
)
)

(defn stars [n]
(apply str (repeat n "*" )
)
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
(if (= (count a-seq) (count (set a-seq))) false true)
)

(defn old-book->new-book [book]
(assoc book :authors (set (:authors book)) )
)

(defn has-author? [book author]
(contains? (:authors book) author)
  )

(defn authors [books]
(apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
(set (map :name (authors books)))
  )

(defn author->string [author]
  (let [n (:name author) dy (:death-year author) by (:birth-year author)]
    (if (contains? author :birth-year) (str n " (" by " - " dy ")") (str n)
      )
    )
)

(defn authors->string [authors]
(apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
(str (:title book) ", written by " (authors->string (:authors book)) )
  )

(defn book->stringWithFullStop [book]
(str (:title book) ", written by " (authors->string (:authors book)) "." )
  )

(defn books->string [books]
 (cond
   (= (count books) 0) (str "No books.")
   (= (count books) 1) (apply str (concat (str "1 book. ") (map book->stringWithFullStop books)))
   :else (apply str (concat (str (count books) " books. ") (interpose " " (map book->stringWithFullStop books))))
  )
  )


(defn books-by-author [author books]
(filter (fn [x] (has-author? x author)) books
  )
)

(defn author-by-name [name authors]
 (first (filter (fn [x] (= (:name x) name)) authors))
  )

(defn living-authors [authors]
(filter (fn [x] (alive? x)) authors)
  )

(defn has-a-living-author? [book]
(not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
(filter has-a-living-author? books))

; %________%
