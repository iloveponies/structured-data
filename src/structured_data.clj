(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
(Math/pow xx xx)
    ))

(defn spiff [v]
  (+ (get v 2) (get v 0) )
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [x (get v 0)
    y (get v 2)]
    (+ x y)))

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
  (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
(let [
        [[x1 y1] [x2 y2]] rectangle
        [p1 p2] point
      ]
   (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [
          [p1 p2] inner
        ]
      (and (contains-point? outer p1) (contains-point? outer p2))
  ))

(defn title-length [book]
  (count(get book :title)))
  

(defn author-count [book]

(count (get book :authors)))

(defn multiple-authors? [book]
(> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
(map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)
    ))

(defn titles [books]
(map :title books)
  )

(defn monotonic? [a-seq]
(or (apply <= a-seq) (apply >= a-seq)
  ))

(defn stars [n]
(apply str (repeat n "*")))

(defn toggle [a-set elem]
(if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))


(defn old-book->new-book [book]
  (assoc book :authors (set(:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
(apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
(let [year (fn [x]
  (cond
    (:death-year x) (str " (" (:birth-year x) " - " (:death-year x) ")")
    (:birth-year x) (str " (" (:birth-year x) " - )")
    :else ""))]
  (str (:name author) (year author))))
    
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
 (str (:title book) ", written by " (authors->string (:authors book)))) 

(defn books->string [books]
  (if (empty? books) "No books." 
    (let [num-books (count books)
          book-str (cond
                     (== 1 num-books) "1 book. "
                     :else (str num-books " books. "))] 
      (str book-str (apply str (interpose ". " (map book->string books))) "."))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
