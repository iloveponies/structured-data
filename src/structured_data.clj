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
  (assoc v (count v) "<3")
)

(defn spiff-destructuring [[v1 v2 v3]]
  (+ v1 v3)
)

(defn point [x y]
  [x y]
)

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]
)

(defn width [[[x1, y1] [x2, y2]]]
  (- x2 x1)
)

(defn height [[[x1, y1] [x2, y2]]]
  (- y2 y1)
)

(defn square? [v]
  (= (height v) (width v))
)

(defn area [v]
  (* (height v) (width v))
)

(defn contains-point? [[[x1, y1] [x2, y2]] [p1, p2]]
  (and
    (<= x1 p1 x2)
    (<= y1 p2 y2)
  )
)

(defn contains-rectangle? [r1 r2]
  (let [[point1 point2] r2]
    (and (contains-point? r1 point1) (contains-point? r1 point2))
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (> (author-count book) 1)
)

(defn add-author [book new-author]
  (assoc book :authors
    (conj (:authors book) new-author)
  )
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let
    [secondval (fn [element] (get element 1))]
    (map secondval collection)
  )
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
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (not 
    (= (count (set a-seq)) (count a-seq))
  )
)

(defn old-book->new-book [book]
  (assoc book :authors
    (set (:authors book))
  )
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (let
    [a (fn [book] (:authors book))]
    (set (apply clojure.set/union (map a books)))
  )
)

(defn all-author-names [books]
  (set
    (map :name (apply clojure.set/union (map :authors books)))
  )
)

(defn author->string [author]
  (let
    [
      name (:name author)
      birth-year (:birth-year author)
      death-year (:death-year author)
      years (cond
        (contains? author :death-year) (str " (" birth-year " - " death-year ")")
        (contains? author :birth-year) (str " (" birth-year " - )")
        :else ""
      )
    ]
    (str name years)
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
    [
      amnt (count books)
      myMap (map book->string books)
    ]
    (cond
      (= amnt 0) "No books."
      (= amnt 1) (str "1 book. " (apply str myMap) ".")
      (> amnt 1) (str amnt " books. " (apply str (interpose ". " myMap)) ".")
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (let
    [a1 (filter (fn [author] (= name (:name author))) authors)]
    (if (= (count a1) 0) nil (first a1))
  )
)

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
)

(defn has-a-living-author? [book]
  (not
    (empty?
      (filter (fn [author] (alive? author)) (authors [book]))
    )
  )
)

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
)

; %________%
