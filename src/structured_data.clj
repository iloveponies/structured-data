(ns structured-data)

(defn do-a-thing [x] (let [xx (+ x x)] (Math/pow xx xx)))

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v] (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle] (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle] (= (width rectangle) (height rectangle)))

(defn area [rectangle] (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point] (let [[[x1 y1] [x2 y2]] rectangle [x y] point] (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner] (let [[point1 point2] inner]
(and (contains-point? outer point1) (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))

(defn alive? [author]
 (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map first(map rest collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
 (apply str(repeat n "*")))

(defn toggle [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (=(count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
 (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (set (get book :authors)) author))

(defn authors [books] (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
 (let [name (if (contains? author :name) (:name author) (str ""))
       birth (if (contains? author :birth-year) 
             	 (str " (" (:birth-year author) " - " 
			(if (contains? author :death-year)
				(:death-year author) (str "")
			)
                  ")") 
		 (str ""))]
 (str name birth)))

(defn authors->string [authors] 
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books] 
(if (= 0 (count books)) "No books." (str (count books) (if (= 1 (count books)) " book. " " books. ") (apply str (interpose ", " (map book->string books))) "." ))  
)

(defn books-by-author [author books]
(filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
(first (filter (fn [x] (= (:name x) name)) authors))  
)

(defn living-authors [authors]
(filter (fn [x] (alive? x)) authors)
)

(defn has-a-living-author? [book]
(not (empty? (living-authors (get book :authors))))
)

(defn books-by-living-authors [books]
(filter (fn [x] (has-a-living-author? x)) books)
)

; %________%
