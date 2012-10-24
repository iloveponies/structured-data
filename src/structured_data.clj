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
    (let [w (- x2 x1)]
      (if (< w 0) (- w) w))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [h (- y2 y1)]
      (if (< h 0) (- h) h))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x3 y3]] rectangle
        [x2 y2] point]
    (and (<= x1 x2 x3) (<= y1 y2 y3))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (not (== (author-count book) 1)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  {:title (:title book)
   :authors (set (:authors book))})

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (contains? author :death-year)
                (str " (" (author :birth-year) " - " (author :death-year) ")")
                (if (contains? author :birth-year)
                  (str " (" (author :birth-year) " - )") ""))]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)]
  	(if (== 0 book-count) "No books."
    	(str book-count " book" (if (> book-count 1) "s" "") ". "
         	(apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (let [f (fn [book] (has-author? book author))]
    (filter f books)))

(defn author-by-name [name authors]
  (let [f (fn [author] (= name (:name author)))
        matches (filter f authors)]
    (if (> (count matches) 0) (first matches) nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))