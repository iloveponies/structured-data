(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
)

(defn spiff [v]
  (+
    (get v 0)
	(get v 2)
  )
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rect]
  (let [[[x1 y1] [x2 y2]] rect]
    (- x2 x1)
  )
)

(defn height [rect]
  (let [[[x1 y1] [x2 y2]] rect]
    (- y2 y1)
  )
)

(defn square? [rect]
  (== (height rect) (width rect))
)

(defn area [rect]
  (*
    (height rect)
	(width rect)
  )
)

(defn contains-point? [rect p]
  (let [[[x1 y1] [x2 y2] [x y]] (conj rect p)]
    (and
      (<= x1 x x2)
      (<= y1 y y2)
	)
  )
)

(defn contains-rectangle? [outer inner]
  (let [[[ix1 iy1] [ix2 iy2]] inner]
    (and
      (contains-point? outer (point ix1 iy1))
	  (contains-point? outer (point ix2 iy2))
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
  (> (count (:authors book)) 1)
)

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)) 
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [getSecond (fn [seq] (get seq 1))]
    (map getSecond collection)
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
	(apply >= a-seq)
  )
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
	(conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (not
    (== (count a-seq) (count (set a-seq)))
  )
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (let [author (fn [book] (:authors book))]
    (set (apply clojure.set/union (map author books)))
  )
)

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(defn author->string [author]
  (let [[name birth-year death-year] [(:name author) (:birth-year author) (:death-year author)]]
    (if (= nil birth-year)
	  name
      (str name " (" birth-year " - " death-year ")")
	)
  )
)

(defn authors->string [authors]
  (apply
    str (interpose ", " (map author->string authors))
  )
)

(defn book->string [book]
  (let [[authors title] [(:authors book) (:title book)]]
    (str title ", written by " (authors->string authors))
  )
)

(defn books->string [books]
  (let [number (count books)]
    (cond 
      (== number 0) "No books."
	  (== number 1) (str number " book. " (apply str (interpose ". " (map book->string books))) ".")
      :else (str number " books. " (apply str (interpose ". " (map book->string books))) ".")
	)
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
)

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
)

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))
  )
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
