(ns structured-data)

(defn do-a-thing [x] (let [xx (+ x x)]
 (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] do (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] do (- y2 y1)))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x3 y3] [x2 y2]] rectangle
	 [x2 y2] point]
   do (if (and (<= x1 x2 x3) (<= y1 y2 y3)) true false))
)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] outer
	[[x3 y3][x4 y4]] inner]
   do (if (and (contains-point? [[x1 y1][x2 y2]][x3 y3]) 
	       (contains-point? [[x1 y1][x2 y2]][x4 y4])) true false))
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
)

(defn alive? [author]
  (if (contains? author :death-year) false true)
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))] (map second collection))
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false)
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq))) false true)
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false)
)

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
   (clojure.set/union (set (map :name (authors books))))
)

(defn author->string [author]
  (let [name (:name author)
	birth (:birth-year author)
	death (:death-year author)] do
  (cond (contains? author :death-year) (str name " (" birth " - " death ")")
	(contains? author :birth-year) (str name " (" birth " - )")
	:else (str name)))
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (let [title (:title book)
	authors (:authors book)] do 
  (str title ", written by " (authors->string authors)))
)

(defn books->string [books]
  (let [number (count books)] do
  (cond (== number 0) (str "No books.") 
   (== number 1) ( str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
   :else (str number " books. " (apply str (interpose ". " (map book->string books))) "."))
))

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
  (if (empty? (filter (fn [author] (alive? author)) (:authors book))) 
      false true)
)

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
)

; %________%
