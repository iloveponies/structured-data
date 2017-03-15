(ns structured-data)

(defn do-a-thing [x]
	(let [xx (+ x x)]
		(Math/pow xx xx)))

(defn spiff [v] (let [third (get v 2)](
	if (not= third nil) (+ (get v 0) third) "?"
)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v] (let[[x y z] v](
	if (not= z nil) (+ x z) "?"
)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] (let [[bl tr] rectangle](
	- (get tr 0) (get bl 0)
)))

(defn height [rectangle] (let [[bl tr] rectangle](
	- (get tr 1) (get bl 1)
)))

(defn square? [rectangle](== (width rectangle) (height rectangle)))

(defn area [rectangle] (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point] (let [[bl tr] rectangle](
	and (<= (get bl 0) (get point 0) (get tr 0)) (<= (get bl 1) (get point 1) (get tr 1))
)))

(defn contains-rectangle? [outer inner] (let[[bl tr] inner](
	and (contains-point? outer tr)(contains-point? outer bl)
)))

(defn title-length [book] (count (get book :title)))

(defn author-count [book] (count (get book :authors)))

(defn multiple-authors? [book] (> (author-count book) 1))

(defn add-author [book new-author] (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author] (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
	(let [second (fn [x] (get x 1))]
		(map second collection)
	)
)

(defn titles [books]
	(map :title books)	
)

(defn monotonic? [a-seq]
	(or(apply <= a-seq)(apply >= a-seq)))

(defn stars [n]
	(apply str (repeat n "*"))
)

(defn toggle [a-set elem]
	(if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
	(if (= (count (distinct a-seq)) (count a-seq)) false true)
)

(defn old-book->new-book [book]
	(assoc book :authors (set (get book :authors)))
)

(defn has-author? [book author]
	(if (some (partial = author) (get book :authors)) true false)
)

(defn authors [books]
    (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
    (set(map :name (authors books))))

(defn author->string [author]
(let[[n b d][(get author :name) (get author :birth-year) (get author :death-year)]]
	(if (nil? b) (str n) (str n " (" b " - " d ")"))
))

(defn authors->string [authors]
	(apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
	(str (get book :title) ", written by " (authors->string (get book :authors)))
)

(defn books->string [books]
	(str (cond (empty? books) "No books" (== (count books) 1) "1 book. " :else (str (count books) " books. ")) (apply str (interpose ", " (map book->string books))) ".")
)

(defn books-by-author [author books]
	(filter (fn [x] (has-author? x author)) books)
)

(defn has-name? [author name]
	(if (some (partial = name) (get author :name)) true false)
)

(defn author-by-name [name authors]
	(first (filter (fn [x] (= (get x :name) name)) authors))
)

(defn living-authors [authors]
	(filter alive? authors)
)

(defn has-a-living-author? [book]
	(if (> (count (living-authors (get book :authors))) 0) true false)
)

(defn books-by-living-authors [books]
	(filter has-a-living-author? books)
)

; %________%
