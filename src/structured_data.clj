(ns structured-data)

(defn do-a-thing [x]
  (let [doubled-x (+ x x)]
    (Math/pow doubled-x doubled-x)))

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

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (==  (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and
     (<= x1 px x2)
     (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and
     (contains-point? outer point1)
     (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors
         (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [a-coll] (get a-coll 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

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
  (not
   (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [{:keys [name birth-year death-year]} author
        year (cond
              death-year (str " (" birth-year " - " death-year ")")
              birth-year (str " (" birth-year " - )" )
              :else "")]
    (str name year)))

(defn authors->string [authors]
  (apply str
         (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [{:keys [title authors]} book]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [num-books (count books)
        book-count (cond
                    (== num-books 0) "No books."
                    (== num-books 1) (str num-books " book")
                    :else (str num-books " books"))]
    (if (> num-books 0)
      (str (apply str
                  (interpose ". "
                             (conj
                              (map book->string books)
                              book-count)))
           ".")
      (str book-count))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [author-list (filter (fn [author] (= (:name author) name)) authors)]
    (first author-list)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
