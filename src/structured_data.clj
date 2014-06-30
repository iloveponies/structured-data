(ns structured-data)

(defn do-a-thing [x]
  (let [dub (+ x x)]
	(Math/pow dub dub)))


(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x, y, z] v]
	(+ x z)))

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
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
		[px py] point]
	(and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
   (let [[[x1 y1] [x2 y2]] inner]
	 (and (contains-point? outer [x1, y1])
		  (contains-point? outer [x1, y2])
		  (contains-point? outer [x2, y1])
		  (contains-point? outer [x2, y2]))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)
		new-authors (conj authors new-author)]
	(assoc book :authors new-authors)))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map
   (fn [coll]
		 (let [[x, y] coll ]
		   y))
   collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
	  (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  ((if (contains? a-set elem)
	disj
	conj) a-set elem))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
	(assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (clojure.set/union (map :name (authors books)))))

(defn author->string [author]
  (let [s1 (:name author)
		s2 (str " ("
				(:birth-year author)
				" - "
				(:death-year author)
				")")]
	(if (:birth-year author)
	  (str s1 s2)
	  s1)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book)
	   ", written by "
	   (authors->string (:authors book))))

(defn books->string [books]
  (let [numBooks (count books)
		bookDescs (apply str (interpose ". " (map book->string books)))]
	(cond (= numBooks 0) "No books."
		  (= numBooks 1) (str "1 book. " bookDescs ".")
		  :else (str numBooks " books. " bookDescs "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [matches (filter (fn [author] (= (:name author) name)) authors)]
	 (cond (= (count matches) 0) nil
		   :else (first matches))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors(:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%

