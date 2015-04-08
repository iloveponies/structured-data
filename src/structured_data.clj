(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (- x2 x1) (- y2 y1)) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
        (if (and (<= x1 p1 x2) (<= y1 p2 y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[v1 z1] [v2 z2]] inner]
        (if (and (contains-point? outer [v1 z1]) (contains-point? outer [v2 z2])) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (not(contains? author :death-year)) true false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-elem (fn [x] (get x 1))]
  	(map get-elem collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq)) true false))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nam (str (:name author))
  	    year1 (str (:birth-year author)) 
  	    year2 (str (:death-year author))]
  	    (cond (contains? author :death-year) (str nam " (" year1 " - " year2 ")") 
  	    	  (and (contains? author :birth-year) (not(contains? author :death-year))) (str nam " (" year1 " - )") 
  	    	  :else nam)))
 
  	    	

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [titl (str (:title book))
  	    authors (authors->string (:authors book))]
  	    (str titl ", written by " authors)))

(defn books->string [books]
  (let [number (count books)
  	    cover (apply str (interpose ". " (map book->string books)))]
  	(cond
  		(== number 0) (str "No books.")
  		(== number 1) (str "1 book. " cover ".") 
  		:else (str number " books. " cover "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [info (filter (fn [x] (= (:name x) name)) authors)]
  	(if (> (count info) 0) (first info)
        (#{nil} nil))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [listOfAuthors (:authors book)]
  	(if (empty? (living-authors listOfAuthors)) false true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
