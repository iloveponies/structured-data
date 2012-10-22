(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
  (Math/pow sum sum)))

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
  (let [ [point1 point2] rectangle [x1 y1] point1 [x2 y2] point2 ]
  (- x2 x1)))

(defn height [rectangle]
  (let [ [point1 point2] rectangle [x1 y1] point1 [x2 y2] point2 ]
  (- y2 y1)))

(defn square? [rectangle]
 (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [ [rect1 rect2] rectangle [x1 y1] rect1 [x2 y2] rect2 [x y] point ]
  (and (<= x1 x x2) (<= y1 y y2))))
  
(defn contains-rectangle? [outer inner]
  (let [ [point1 point2] inner ]
  (and (contains-point? outer point1) (contains-point? outer point2))))

(defn title-length [book]
  (let [ title (:title book) ] 
  (count title)))

(defn author-count [book]
  (let [authors (:authors book) ]
  (count authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))
  
(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [ x (fn [y] (get y 1)) ]
  (map x collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors (map old-book->new-book books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [ life (fn [x]
    (cond
     (:death-year x) (str " (" (:birth-year x) " - " (:death-year x) ")")
     (:birth-year x) (str " (" (:birth-year x) " - )")
     :else ""))]
  (str (:name author) (life author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [ x (count books) ]
   (cond
    (== x 0) "No books."
    (== x 1) (str "1 book. " (book->string (first (seq books)))".")
    :else (str x " books. " (apply str (interpose ". " (map book->string books)))"."))))

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
