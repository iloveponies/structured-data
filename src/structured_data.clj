(ns structured-data)

(defn do-a-thing [x]
  (let [common-name (+ x x)]
  	(Math/pow common-name common-name)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
  		  [x y] point]
  	(and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
  	(and (contains-point? outer point1)
  		   (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [new (conj (:authors book) new-author)]
  	(assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
  	(map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
  	  (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  	(disj a-set elem)
  	(conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
	(apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth (:birth-year author)
  		  years (if birth
  				      (str " (" birth " - " (:death-year author) ")")
  				      "")]
  	(str (:name author) years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [books-amount (count books)
        books-string (str (apply str (interpose ", " (map book->string books))) ".")]
    (cond
      (== books-amount 0) "No books."
      (== books-amount 1) (str "1 book. " books-string)
      :else (str books-amount " books. " books-string))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
  