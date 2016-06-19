(ns structured-data)

(defn do-a-thing [x]
  (let [tuplax (+ x x)]
  	(Math/pow tuplax tuplax)))

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
  (let [[[x1 y1][x2 y2]] rectangle]
  	(Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
  	(Math/abs (- y1 y2))))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
  	true
  	false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle [x y] point]
  	(if (and
  		(<= x1 x x2)
  		(<= y1 y y2))
  	true
  	false)))

(defn contains-rectangle? [outer inner]
  (let [[bottom top] inner]
  	(if (and
  		(contains-point? outer bottom)
  		(contains-point? outer top))
  	true
  	false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1)
  	true
  	false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
  		(assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (contains? author :death-year)
  	false
  	true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
	(let [sloppify (fn [v] (get v 1))]
		(map sloppify collection)))

(defn titles [books]
  (let [titlefy (fn [book] (:title book))]
  	(map titlefy books)))

(defn monotonic? [a-seq]
  (if (or
  	(apply <= a-seq)
  	(apply >= a-seq))
  true
  false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  	(disj a-set elem)
  	(conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq)))
  	false
  	true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
  	true false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author) yrs (str "(" (:birth-year author) " - " (:death-year author) ")")]
  	(if (contains? author :birth-year)
  		(str name " " yrs)
  		name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookstring (apply str (interpose ". " (map book->string books)))]
  	(cond
  		(= (count books) 0) "No books."
  		(= (count books) 1) (str "1 book. " bookstring ".")
  		:else (str (count books) " books. " bookstring "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
  	false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
