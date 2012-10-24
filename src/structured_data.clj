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
  (let [[[x1,y1] [x2,y2]] rectangle]
		(if (< x1 x2) (- x2 x1) (- x1 x2))))

(defn height [rectangle]
  (let [[[x1,y1] [x2,y2]] rectangle]
		(if (< y1 y2) (- y2 y1) (- y1 y2))))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle))
  )

(defn area [rectangle]
	(* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1,y1] [x2,y2]] rectangle]
		(let [[findx,findy] point]
          (boolean (and (<= x1 findx x2) 
               			(<= y1 findy y2))))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
		 (contains-point? outer p2))))

(defn title-length [book]
  (let [title (:title book)]
    (count title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
	(let [old-authors (get book :authors)]
		(assoc book :authors (conj old-authors new-author))))

(defn alive? [author]
	(not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
	(map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq))))) 

(defn old-book->new-book [book]
	(let [old-authors (get book :authors)]
  		(assoc book :authors (set old-authors))))

(defn has-author? [book author]
  (let [bauthor (get book :authors)]
    (contains? bauthor author)))

(defn authors [books]
  (let [get-authors
        (fn [book] (:authors book))]
    (apply clojure.set/union (map get-authors books))))

(defn all-author-names [books]
  (let [writers (authors books)]
	(set (map :name writers))))

(defn author->string [author]
  (let [nam (:name author)
    byear (:birth-year author)
    dyear (:death-year author)]
    (str nam (if dyear 
               (str " (" byear " - " dyear ")")
               (if byear 
                 (str " (" byear " - " ")"))))))  

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        writers (:authors book)]
    (str title ", written by " (authors->string writers))))

(defn books->string [books]
	(let [amount (count books)]
      (str (if (== amount 0) "No books"
  		(apply str (count books) " book"
           (if (> amount 1) "s")
       			". " (interpose ". " (map book->string books))))
        (str "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (if (= (:name (first authors)) name) 
    	(first authors)
    	(if (> (count authors) 0)
            (author-by-name name (rest authors))
          	nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (< 0 (count (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))