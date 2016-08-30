(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx))
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
  (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle] (let [[x3 y3] point]
  (and (<= x1 x3 x2) (<= y1 y3 y2)))
  ))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
  (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn elementLength [element]
  (count element))

(defn element-lengths [collection]
  (map elementLength collection))

(defn second-elements [collection]
  (let [sequ (fn [x] (get x 1))]
	(map sequ collection)))

(defn titles [books]
  (let [getTitle (fn [book] (get book :title))]
	(map getTitle books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (distinct a-seq))))
)
(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors)))
)

(defn has-author? [book author]
  (contains? (book :authors) author)
)

(defn authors [books]
  (let [getAuthors (fn [book] (get book :authors))]
	(apply clojure.set/union (map getAuthors books)))
)

(defn all-author-names [books]
  (let [getNames (fn [author] (get author :name))]
  (set (map getNames (authors books))))
)

(defn author->string [author]
  (if (contains? author :death-year) 
	(str (author :name) " (" (author :birth-year) " - " (author :death-year) ")")
	(if (contains? author :birth-year)
		(str (author :name) " (" (author :birth-year) " - )")
		(str (author :name))
	)
  )
	)

(defn authors->string [authors]
  (let [toString (fn [author] (author->string author))]
	
  (apply str (interpose ", " (set (map toString authors)))))
  )

(defn book->string [book]
	(apply str (get book :title) ", written by "(authors->string (get book :authors)))
)

(defn books->string [books]
	(let [toString (fn [book] (apply str ". "(book->string book)))]
		(cond
			(== (count books) 0) "No books."
			(== (count books) 1) (apply str "1 book" (apply str (map toString books)) ".")
			:else (apply str (count books) " books" (apply str (map toString books)) ".")
		)
	)
)

(defn books-by-author [author books]
	(filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
	(if (== (count (filter (fn [author] (= (author :name) name)) authors)) 0) nil (first (filter (fn [author] (= (author :name) name)) authors)))
)

(defn living-authors [authors]
	(filter alive? authors)
)

(defn has-a-living-author? [book]
	(> (count (living-authors (book :authors))) 0)
)

(defn books-by-living-authors [books]
	(filter has-a-living-author? books)
)

; %________%
