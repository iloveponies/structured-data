(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)
	))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
	))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
    (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [outer [point1 point2]]
   (and (contains-point? outer point1)
		(contains-point? outer point2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors
	(conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
	  (apply >= a-seq)
	))

(defn stars [n]
  (apply str (repeat n \*) ))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
		(disj a-set elem)
		(conj a-set elem)
	))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author-string-ending [author]
	(str " (" (:birth-year author) " - "
		(if (contains? author :death-year) (:death-year author) "") ")"))
  
(defn author->string [author]
  (str (:name author)
	  (if (contains? author :birth-year)
		(author-string-ending author) "")))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn book-count [books]
	(cond
		(= (count books) 1) "1 book. "
		(= (count books) 0) "No books"
		:else (str (count books) " books. ")))

(defn books->string [books]
  (str (book-count books)
	(apply str (interpose ". " (map book->string books))) "."))

(defn books-by-author [author books]
  (filter
	(fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first
	(filter
		(fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
