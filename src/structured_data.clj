(ns structured-data)

(defn do-a-thing [x]
  (let [z (+ x x)]
    (Math/pow z z)))

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

(defn width [[[x1 y1] [x2 y2]]]
	(if (> x2 x1)
      (- x2 x1)
      (- x1 x2)))

(defn height [[[x1 y1] [x2 y2]]]
  	(if (> y2 y1)
      (- y2 y1)
      (- y1 y2)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]][x y]]
  (and
   (<= x1 x x2)
   (<= y1 y y2)))

(defn contains-rectangle? [outer [point1 point2]]
  (and
   (contains-point? outer point1)
   (contains-point? outer point2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (let [count (author-count book)]
    (> count 1)))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (:authors book)
          new-author)))

(defn alive? [author]
  (not
   (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [lolwtf
        (fn [[-- secondE]] secondE)]
    (map lolwtf collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (apply <= a-seq)
    true
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not=
   (count a-seq)
   (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors
    (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name 
       (authors books))))

(defn author->string [author]
  (let [years (fn [birth death] (cond
                                 (not= nil death) (str " (" birth " - " death ")")
                                 (not= nil birth) (str " (" birth " - )")))]
  (str (:name author) (years (:birth-year author) (:death-year author)))))

(defn authors->string [authors]
  (let [colonedAuth (interpose "; " 
                    (map author->string authors)) index (.lastIndexOf colonedAuth ", ")]
  (apply str 
         (if (= index -1)
           colonedAuth
           (assoc (vec colonedAuth) index " and ")))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [addDots (fn [sequ] (apply str (interpose "; " sequ)))]
  (cond
   (empty? books) "No books."
   (= (count books) 1) (str "1 book. " (book->string (get books 0)))
   :else (str (count books) " books. " (addDots (map book->string books))))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name2 authors]
  (let [theBeatDrop (fn [author] (= (:name author) name2))]
    (first (filter theBeatDrop authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))