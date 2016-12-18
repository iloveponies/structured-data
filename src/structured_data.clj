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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner ]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2]))))

(defn title-length [book]
 (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original  (:authors book)
        updated   (conj original new-author)]
    (assoc book :authors updated)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secondelem (fn [x] (get x 1))]
    (map secondelem collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \* )))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem) ;true
    (conj a-set elem) ;false
    ))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count(set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        byear (:birth-year author)
        dyear (:death-year author)]
    (str name (cond
                (not (= dyear nil)) (str " (" byear " - " dyear ")")
                (not (= byear nil)) (str " (" byear " - )" )
                :else ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books) 
      "No books."
      (str (count books) " books. "
             (apply str (interpose ". " (map book->string books))) "." )))
    

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (filter (fn [author] (= (:name author) name)) authors))

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
