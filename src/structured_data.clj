(ns structured-data)

(defn do-a-thing [x]
  (let [d (+ x x)]
       (Math/pow d d)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
        (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
            (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
            (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
                      (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [outer [bottom-left top-right]]
                      (and (contains-point? outer bottom-left) (contains-point? outer top-right)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
    (let [new-authors (conj (:authors book) new-author)]
         (assoc book :authors new-authors)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [snd (fn [[x y]] y)]
       (map snd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nm (:name author)
        by (:birth-year author)
        dy (:death-year author)]
       (cond dy (str nm " (" by " - " dy ")")
             by (str nm " (" by " - )")
             :else nm)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [cnt (count books)]
       (cond (= cnt 0) "No books."
             (= cnt 1) (str "1 book. " (book->string (first books)) ".")
             :else     (str cnt " books. " (apply str (interpose ", " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
