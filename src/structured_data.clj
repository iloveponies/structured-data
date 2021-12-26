(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x _ y]]
  (+ x y))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _] [x2 _]]]
  (- x2 x1))

(defn height [[[_ y1] [_ y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x3 y3]]
  (and (<= x1 x3 x2)  (<= y1 y3 y2)))

(defn contains-rectangle? [outer [point1 point2]]
  (and (contains-point? outer point1) (contains-point? outer point2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [{authors :authors}]
  (count authors))

(defn multiple-authors? [book]
  (let [{authors :authors} book]
    (> (count authors) 1)))

(defn add-author [book new-author]
  (let [new (conj (:authors book) new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (not (boolean (:death-year author))))

(defn element-lengths [collection]
  (let [len (fn [x] (count x))]
    (map len collection)))

(defn second-elements [collection]
  (let [scnd (fn [x] (get x 1))]
    (map scnd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (elem a-set)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [new (set (:authors book))]
    (assoc book :authors new)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [{name :name
         birth :birth-year
         death :death-year} author]
    (str name (if birth
                (str " (" birth " - " death ")")
                ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [{:keys [title authors]} book]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (str (cond (empty? books) "No books"
             (= 1 (count books)) "1 book. "
             :else (str (count books) " books. "))
       (apply str (interpose ". " (map book->string books)))
       "."))

(defn books-by-author [author books]
  (let [is-author? (fn [x] (has-author? x author))]
    (filter is-author? books)))

(defn author-by-name [name authors]
  (let [is-named? (fn [x] (= name (:name x)))]
    (first (filter is-named? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (boolean (first (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

                                        ; %________%
