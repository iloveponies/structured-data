(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
   (Math/pow xx xx)
  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y] [(get v 0) (get v 2)]]
    (+ x y)
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)
  )
)

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)
  )
)

(defn square? [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (if (== (- x2 x1) (- y2 y1)) true false)
  )
)

(defn area [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (* (- y2 y1) (- x2 x1))
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle [px py] point]
    (if (and (<= x1 px x2) (<= y1 py y2)) true false)
  )
)

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if (and (contains-point? outer p1)
             (contains-point? outer p2))
      true
      false
    )
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false)
)

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))
  )
)

(defn alive? [author]
  (if (:death-year author) false true)
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
    (map second-element collection)
  )
)

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq)
          (apply >= a-seq)
       )
    true
    false
  )
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
  (if (== (count (set a-seq)) (count a-seq))
    false
    true
  )
)

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))
  )
)

(defn has-author? [book author]
  (let [authors (:authors book)]
    (if (contains? authors author) true false)
  )
)

(defn authors [books]
  (let [authorsofbook (fn [book] (:authors book))] ;set
    (apply clojure.set/union (map authorsofbook books))
  )
)

(defn all-author-names [books]
  (let [authorset (authors books)]
    (set (map :name authorset))
  )
)

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if(= birth nil)
      (str name)
      (str name " (" birth " - " death ")")
    )
  )
)



(defn authors->string [authors]
  (apply str (interpose ", " (set (map author->string authors))))
)

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))
  )
)

(defn books->string [books]
  (let [amount (count books)]
    (cond
      (== amount 0) (str "No books.")
      (== amount 1) (str amount " book. " (book->string (get books 0)) ".")
      (> amount 1)  (str amount " books. " (apply str (interpose ", " (map book->string books))) ".")
    )
  )
)

(defn books-by-author [author books]
  (let [has-author (fn [book] (if (has-author? book author) book nil))
        !nil (fn [item] (if (= item nil) false true))]
    (filter !nil (map has-author books))
  )
)

(defn author-by-name [name authors]
  (let [found-author? (fn [author] (if (= name (:name author)) author nil))
        !nil (fn [item] (if (= item nil) false true))
        mappi (map found-author? authors)]

    (if (== (count (filter !nil mappi)) 0) nil (first (filter !nil mappi)))
  )
)

(defn living-authors [authors]
  (let [alive (fn [author] (if (alive? author) author nil))]
    (filter (complement nil?) (map alive authors))
  )
)

(defn has-a-living-author? [book]
  (if (> (count (living-authors (:authors book))) 0) true false)
)

(defn books-by-living-authors [books]
  (let [construct-map (fn [book]
                      (if (has-a-living-author? book)
                       book nil))]
    (filter (complement nil?) (map construct-map books))
  )
)
; %________%
