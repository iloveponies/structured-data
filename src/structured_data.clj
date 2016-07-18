(ns structured-data)

(defn do-a-thing [x]
	(let [xx (+ x x)]
		(Math/pow xx xx)
		)
	)

(defn spiff [v]
	(let [a (get v 0)
		  b (get v 2)]
		(+ a b)
		)
	)

(defn cutify [v]
	(conj v "<3")
	)

(defn spiff-destructuring [v]
	(let [[a _ b] v]
		(+ a b)
		)
	)

(defn point [x y]
	[x y])

(defn rectangle [bottom-left top-right]
	[bottom-left top-right])

(defn width [rectangle]
	(let [[[x1 y1] [x2 y2]] rectangle]
		(Math/abs (- x1 x2))
		)
	)

(defn height [rectangle]
	(let [[[x1 y1] [x2 y2]] rectangle]
		(Math/abs (- y1 y2))
		)
	)

(defn square? [rectangle]
	(= (height rectangle) (width rectangle)
	   )
	)

(defn area [rectangle]
	(* (height rectangle) (width rectangle))
	)

(defn contains-point? [rectangle point]
	(let [[[x1 y1] [x2 y2]] rectangle
		  [x y] point]
		(and
			(<= x1 x x2)
			(<= y1 y y2)
			)
		)
	)

(defn contains-rectangle? [outer inner]
	(let [[p1 p2] inner]
		(and
			(contains-point? outer p1)
			(contains-point? outer p2)
			)
		)
	)

(defn title-length [book]
	(count (:title book)
		   )
	)

(defn author-count [book]
	(count (:authors book)
		   )
	)

(defn multiple-authors? [book]
	(< 1 (author-count book)
	   )
	)

(defn add-author [book new-author]
	(let [old-authors (:authors book)
		  new-authors (conj old-authors new-author)]
		(assoc book :authors new-authors)
		)
	)

(defn alive? [author]
	(not (contains? author :death-year)
		 )
	)

(defn element-lengths [collection]
	(map count collection)
	)

(defn second-elements [collection]
	(map second collection)
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
	(apply str (repeat n "*")
		   )
	)

(defn toggle [a-set elem]
	(if (contains? a-set elem)
		(disj a-set elem)
		(conj a-set elem)
		)
	)

(defn contains-duplicates? [a-seq]
	(not=
		(count (set a-seq))
		(count a-seq))
	)

(defn old-book->new-book [book]
	(assoc book :authors (set (:authors book))
		)
	)

(defn has-author? [book author]
	(contains? (:authors book) author)
	)

(defn authors [books]
	(apply clojure.set/union (map :authors books)
		   )
	)

(defn all-author-names [books]
	(set (map :name (authors books))))

(defn author->string [author]
	(let [name (:name author)
		  years (str "(" (:birth-year author) " - " (:death-year author) ")")
		  ]
		(if (contains? author :birth-year)
			(str name " " years)
			name
			)
		)
	)

(defn authors->string [authors]
	(apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
	(str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
	(let [c (count books)
		  pretty-count (cond
						   (= 0 c) "No books."
						   (= 1 c) "1 book."
						   (< 1 c) (str c " books.")
						   )
		  pretty-books (apply str (map #(str " " (book->string %) ".") books))
		  ]
		(str pretty-count pretty-books)
		)
	)

(defn books-by-author [author books]
	(filter #(has-author? % author) books)
	)

(defn author-by-name [name authors]
	(first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
	(filter alive? authors))

(defn has-a-living-author? [book]
	(not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
	(filter has-a-living-author? books))

; %________%
