(ns structured-data)

(defn do-a-thing [x]
  (let [xpx (+ x x)]
    (Math/pow xpx xpx)
    ))

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
       (Math/abs (- x1 x2)))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2)))
  )

(defn square? [rectangle]
(== (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2 ))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner
        cp contains-point?]
    (and (cp outer p1) (cp outer p2))
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (>= (author-count book) 2)
  )

(defn add-author [book new-author]
  (let [auth (:authors book)]
    (assoc book :authors (conj auth new-author))
    )
  )

(defn alive? [author]
  (not  (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [scnd (fn [v] (get v 1))]
    (map scnd collection))
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq))
  )

(defn old-book->new-book [book]
  (let [ob (:authors book)]
        (assoc book :authors (set ob)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
 )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
        dy (:death-year author)
        by (:birth-year author)
        ]
    (str name (if (or dy by) (str " (" by " - " dy ")") ""))
    )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [authstring (authors->string (:authors book))
        title (:title book)]
    (str title ", written by " authstring))
  )

(defn books->string [books]
  (let [nbooks (count books)]
    (cond (> nbooks 1) (str nbooks " books. " (apply str (interpose ", " (map book->string books))) ".")
          (== nbooks 1) (str nbooks " book. " (apply str (interpose ", " (map book->string books))) ".")
      :else "No books."
      ))
  )

(defn books-by-author [author books]
  (let [ha (fn [book] (has-author? book author))]
    (filter ha books))
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)
         )
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (if (> (count (living-authors (:authors book))) 0) true false)
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
