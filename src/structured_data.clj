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
    (== (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and (<= x1 x3 x4 x2) (<= y1 y3 y4 y2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        new (conj authors new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [funk (fn [x] (get x 1))]
    (map funk collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (clojure.set/union (map :name (authors books)))))

(defn author->string [author]
  (cond
   (contains? author :death-year) (str (author :name) " (" (author :birth-year) " - " (author :death-year) ")")
   (contains? author :birth-year) (str (author :name) " (" (author :birth-year) " - )")
   (contains? author :name) (:name author)
   :else (str ", ")))

(defn authors->string [authors]
  (let [aut (interpose ", " authors)] 
  (if (< 0 (count authors)) 
    (apply str (map author->string aut)) 
           (str ""))))

(defn book->string [book]
  (str (book :title) ", written by " 
       (authors->string (book :authors))))

(defn books->string [books]
  (cond
   (== 0 (count books)) (str "No books.")
   (== 1 (count books)) (str "1 book. " (book->string 
                                         (first books)) ".")
   :else (str (count books) " books. " 
              (apply str (interpose ". " 
                                    (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (let [filt (filter (fn [a] (= (:name a) name)) authors)]
  (if (< 0 (count filt)) (first filt) (or))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))