(ns structured-data)

(defn do-a-thing [x]
	(let [a (+ x x)]
		(Math/pow a a)))


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
	(* (height rectangle) (width rectangle))
)

(defn contains-point? [rectangle point]
	(let [[[x1 y1] [x2 y2]] rectangle
		[x y] point
		]
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
	(count (get book :title))
)

(defn author-count [book]
	(count (get book :authors))
)

(defn multiple-authors? [book]
	(> (author-count book) 1)
)

(defn add-author [book new-author]
	(let [b (get book :authors)]
		(assoc book :authors
			(conj b new-author)
		)
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
		[scnd (fn [x] (get x 1))]
		(map scnd collection)
	)
)

(defn titles [books]
	(let
		[gt (fn [x] (get x :title))]
		(map gt books)
	)
)

(defn monotonic? [a-seq]
	(apply
		(if (> (get a-seq 0) (get a-seq 1)) >= <=)
		a-seq
	)
)

(defn stars [n]
	(apply str (repeat n "*"))
)

(defn toggle [a-set elem]
	((if (contains? a-set elem) disj conj)
		a-set
		elem
	)
)

(defn contains-duplicates? [a-seq]
	(not (= (count a-seq) (count (set a-seq))))
)

(defn old-book->new-book [book]
	(assoc book :authors
		(set (get book :authors))
	)
)

(defn has-author? [book author]
	(contains?
		(get (old-book->new-book book) :authors)
		author
	)
)

(defn authors [books]
	(apply clojure.set/union
		(map (fn [x] (get x :authors)) books)
	)
)

(defn all-author-names [books]
	(set 
		(map (fn [x] (get x :name))
			(authors books)
		)
	)
)

(defn author->string [author]
	(let [
		name (get author :name)
		by (get author :birth-year)
		dy (get author :death-year)
		]
		(str
			name
			(if by (str " (" by " - " dy ")"))
		)
	)
)

(defn authors->string [authors]
	(apply str
		(interpose
			", "
			(map author->string authors)
		)
	)
)

(defn book->string [book]
	(apply str
		(get book :title)
		", written by "
		(authors->string (get book :authors))
	)
)

(defn books->string [books]
	(let [n (count books)] 
		(str
			(cond
				(= n 0) "No books"
				(= n 1) "1 book. "
				:else (str n " books. ")
			)
			(str
				(apply str
					(interpose
						". "
						(map book->string books)
					)
				)
				"."
			)
		)
	)
)

(defn books-by-author [author books]
	(filter
		(fn [x] (contains? (get x :authors) author))
		books
	)
)

(defn author-by-name [name authors]
	(first
		(filter
			(fn [x] (= (get x :name) name))
			authors
		)
	)
)

(defn living-authors [authors]
	(filter alive? authors)
)

(defn has-a-living-author? [book]
	(not
		(empty?
			(living-authors
				(get book :authors)
			)
		)
	)
)

(defn books-by-living-authors [books]
	(filter has-a-living-author? books)
)

; %________%
