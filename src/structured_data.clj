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
	(== (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    	[x3 y3] point]
	(and (>= x2 x3 x1) (>= y2 y3 y1))))

(defn contains-rectangle? [outer inner]
	(let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [ x (:authors book)]
	(assoc book :authors (conj x new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [collection] (get collection 1))]
	(map get-second collection)))

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
	(let [x (:authors book)]
                      (assoc book :authors (set x))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]  
  (let [author (fn [book] (:authors book))]
  (apply clojure.set/union (map author books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimike (:name author)
		synty (:birth-year author)
        kuolo (:death-year author)]
  	(str nimike (if synty (str " (" synty " - " (if kuolo (str kuolo)) ")")))))

(defn authors->string [authors]
	(apply str (interpose ",  " (map author->string authors))))

(defn book->string [book]
  (let [auktoorit (:authors book)
        titteli (:title book)]
	(str titteli ", written by " (authors->string auktoorit))))

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)