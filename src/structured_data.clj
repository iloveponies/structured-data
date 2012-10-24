(ns structured-data)

(defn do-a-thing [x]
  (let [s (+ x x)]
    (Math/pow s s)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[a b] inner]
    (and (contains-point? outer a) (contains-point? outer b))
   )
  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [a (conj (:authors book) new-author)]
    (assoc book :authors a)
  )
 )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [a (fn [v] (get v 1))]
    (map a collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [a (set (:authors book))]
    (assoc book :authors a)
  )
 )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nam (:name author)
        by (:birth-year author)
        dy (str (:death-year author))
        y (str " (" by " - " dy ")" )]
	(str nam (if by y ""))
    )
  )

(defn authors->string [authors]
  ( apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
     (empty? books) "No books."
     :else (let [n (count books)
                 plur (if (= 1 n) (str n " book.") (str n " books."))
                 bookstr (fn [book] (str " " (book->string book) "."))]
              (str plur 
                   (apply str 
                          (apply concat (map bookstr books))
                    )
               )
            )
   )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))