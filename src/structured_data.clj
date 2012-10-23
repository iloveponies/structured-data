(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)] 
    (Math/pow xx xx)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)] (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
    (spiff v))

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
    (cond
     (== (- x2 x1) (- y2 y1)) true
     :else false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (*(- x2 x1)(- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [a1 a2] point]
    (cond
     (and (<= x1 a1 x2) (<= y1 a2 y2)) true
     :else false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[a1 b1][a2 b2]] inner]
    (cond 
     (and (contains-point? outer [a1 b1]) (contains-point? outer [a2 b2])) true
     :else false
     )))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (let [x (author-count book)]
  (cond
   (< 1 x) true
   :else false)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-el (fn [x] (get x 1))]
    (map second-el collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n \*)))

(defn toggle [a-set elem]
  (cond 
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond
     (contains? author :birth-year) 
     (str (:name author) " (" (:birth-year author)
          " - " (or (:death-year author) "") ")" )
     :else (str (:name author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " 
        (authors->string (:authors book))))

(defn books->string [books]
(let [x (count books)]
  (cond
   (== 0 x) "No books."
   :else (str x " book" 
              (if (> x 1) "s") ". " 
              (apply str (interpose ", " 
                                    (map book->string books))
   )"."))))

(defn books-by-author [author books]
  (let [has-author (fn [book] (has-author? book author))]
    (filter has-author books)))

(defn author-by-name [name authors]
  (let [auth-name (fn [author] (= name (:name author)))] 
    (first (filter auth-name authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))