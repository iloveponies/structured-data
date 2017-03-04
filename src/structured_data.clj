(ns structured-data)

(defn do-a-thing [x]
  (let [double-it (+ x x)]
    (Math/pow double-it double-it)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[one _ three] v] (+ one three)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _] [x2 _]]]
  (- x2 x1))

(defn height [[[_ y1] [_ y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[rect-x1 rect-y1] [rect-x2 rect-y2]] [point-x point-y]]
  (and (>= point-x rect-x1) 
       (>= point-y rect-y1) 
       (<= point-x rect-x2) 
       (<= point-y rect-y2)))

(defn contains-rectangle? [outer [[inner-x1 inner-y1] [inner-x2 inner-y2]]]
  (and (contains-point? outer [inner-x1 inner-y1]) 
       (contains-point? outer [inner-x2 inner-y2])))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [coll] (get coll 1))] 
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) 
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem) 
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [has-birth-year?
          (fn [author] (not (= (:birth-year author) nil)))                  
        author-years
          (fn [author] (if (has-birth-year? author)
                         (str " (" (:birth-year author) " - " (:death-year author) ")")
                         (str "")))]            
    (str (:name author) (author-years author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)]
  (cond
    (= book-count 0)  "No books."
    (= book-count 1)  (str "1 book. " (book->string (get books 0)) ".")
    :else             (str book-count " books. " (apply str (interpose ". " (map book->string books))))))) 

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (set (living-authors (:authors book))))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
