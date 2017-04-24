(ns structured-data)

(defn do-a-thing [x]
    (let [y (+ x x)]
        (Math/pow y y)))

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
    (= (width rectangle) (height rectangle)))

(defn area [rectangle]
    (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
    (let [[x y] point
        [[x1 y1] [x2 y2]] rectangle]
        (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
    (let [[p1 p2] inner]
        (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
    (count (get book :title)))

(defn author-count [book]
    (count (:authors book)))

(defn multiple-authors? [book]
    (> (author-count book) 1))

(defn add-author [book new-author]
    (let [new-author-list (conj (:authors book) new-author)]
        (assoc book :authors new-author-list)))

(defn alive? [author]
    (not (contains? author :death-year)))

(defn element-lengths [collection]
    (map count collection))

(defn second-elements [collection]
    (let [sec (fn [v] (get v 1))]
        (map sec collection)))

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
    (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
    (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
    (contains? (:authors book) author))

(defn authors [books]
    (set (apply concat (map :authors books))))

(defn all-author-names [books]
    (set (map :name (authors books))))

(defn author->string [author]
    (let [an (:name author)
          by (:birth-year author)
          dy (:death-year author)]
        (if (contains? author :birth-year)
            (str an " (" by " - " dy ")")
            an)))

(defn authors->string [authors]
    (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
    (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
    (let [n (count books)
          s (if (> n 1) "s" "")]
        (if (= n 0)
            "No books."
            (str n " book" s ". " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
    (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
    (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
    (filter alive? authors))

(defn has-a-living-author? [book]
    (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
    (filter has-a-living-author? books))

; %________%
