(ns structured-data)

(defn do-a-thing [x]
	(let [xx (* x 2)]
		(Math/pow xx xx)))

(defn spiff [v]
	(let [f (get v 0) t (get v 2)]
		(+ f t)))

(defn cutify [v]
	(conj v "<3"))

(defn spiff-destructuring [v]
	(let [ [f _ t] v ]
		(+ f t)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
	(let [ [[x1 y1] [x2 y2]] rectangle ]
		(Math/abs (- x2 x1))))

(defn height [rectangle]
	(let [ [[x1 y1] [x2 y2]] rectangle ]
		(Math/abs (- y2 y1))))

(defn square? [rectangle]
	(= (height rectangle) (width rectangle)))

(defn area [rectangle]
	(* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
	(let [ [[x1 y1] [x2 y2]] rectangle
				 [px py] point ]
		(and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [rect inner]
  (every? #(contains-point? rect %1) inner))

(defn title-length [book]
	(count (:title book)))

(defn author-count [book]
	(count (:authors book)))

(defn multiple-authors? [book]
	(> (count (:authors book)) 1))

(defn add-author [book new-author]
	(let [authors (:authors book)
				updated-authros (conj authors new-author)]
		(assoc book :authors updated-authros)))

(defn alive? [author]
	(not (contains? author :death-year)))

(defn element-lengths [collection]
	(map count collection))

(defn second-elements [collection]
	(map second collection))


(defn titles [books]
	(map :title books))

(defn monotonic? [a-seq]
	(or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
	(apply str (repeat n "*")))

(defn toggle [a-set elem]
	(if (contains? a-set elem)
		(disj a-set elem)
		(conj a-set elem)))

(defn contains-duplicates? [a-seq]
	(if (set? a-seq)
		false ;; inherent to sets not having duplicates
		(not= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
	(let [authors (:authors book)]
		(assoc book :authors (set authors))))

(defn has-author? [book author]
	(contains? (:authors book) author))

(defn authors [books]
	(apply clojure.set/union (map #(:authors %1) books)))

(defn all-author-names [books]
	(set (map :name (authors books))))

(defn author->string [author]
	(let [name (:name author)
				birth-year (:birth-year author)
				death-year (:death-year author)]
		(cond
			(nil? birth-year) name
			(nil? death-year) (str name " (" birth-year " - )")
			:else (str name " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
	(apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
	(let [title (:title book)
				authors (:authors book)]
		(str title ", written by " (authors->string authors))))

(defn books->string [books]
	(let [ books-qt (count books)
				 process-books-string #(apply str (interpose ". " (map book->string %1))) ]
		(cond
			(= 0 books-qt) "No books."
			(= 1 books-qt) (str "1 book. " (book->string (first books)) ".")
			:else (str books-qt " books. " (process-books-string books) "."))))

(defn books-by-author [author books]
	(filter #(has-author? %1 author) books))

(defn author-by-name [name authors]
	(first (filter #(= name (:name %1)) authors)))

(defn living-authors [authors]
	(filter #(nil? (:death-year %1)) authors))

(defn has-a-living-author? [book]
;; 	(> (count (living-authors (:authors book))) 0))
	(-> (:authors book)
			(living-authors)
			(count)
			(>= 1)))

(defn books-by-living-authors [books]
	(filter has-a-living-author? books))

; %________%
