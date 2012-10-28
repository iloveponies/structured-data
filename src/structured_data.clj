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
  (== (width rectangle) (height rectangle))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and 
     (contains-point? outer point1) 
     (contains-point? outer point2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [book-authors (get book :authors)]	
    (assoc book :authors (conj book-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let[return-second (fn [col] (get col 1))]
    (map return-second collection)))

(defn titles [books]
  (let[get-title (fn [book] (get book :title))]
    (map get-title books)))

(defn monotonic? [a-seq]
  (or 
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> 
   (count a-seq)
   (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name-string (:name author)
        years (if (contains? author :birth-year)
                (str " (" (:birth-year author) " - " 
                     (:death-year author) ")"))]
    (str name-string years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str 
         (interpose ", written by " 
                  [(:title book) 
                   (authors->string (:authors book))])))

(defn books->string [books]	
  (let [book-count (count books)]
    (str (if (== book-count 0)
      "No books"
      (if (== book-count 1)
        "1 book. "
          (str book-count " books. ")))
    (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (let [f 
        (fn [book] 
           (if (has-author? book author)
             book))]
  (filter f books)))

(defn author-by-name [name authors]
  (let [f 
        (fn [author] 
          (if (= name (:name author))
            author
            nil))]
    (first (filter f authors))))

(defn living-authors [authors]
  (let [f (fn [author] 
            (if (alive? author)
              author))]
  (filter f authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (let [f (fn [book] 
            (if (has-a-living-author? book)
              book))]
  (filter f books)))