(ns structured-data)

(defn do-a-thing [x]
  (let [doubled (+ x x)]
    (Math/pow doubled doubled)))

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
  (let [[[x1, y1] [x2, y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1, y1] [x2, y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1, ry1] [rx2, ry2]] rectangle
        [x, y] point]
    (and (<= rx1 x rx2) (<= ry1 y ry2))))

(defn contains-rectangle? [outer inner]
   (let [[top-left bottom-right] inner]
    (and (contains-point? outer top-left) (contains-point? outer bottom-right))))

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
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [counter (fn [x] (get x 1))]
    (map counter collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
     (apply <= a-seq) true
     (apply >= a-seq) true
     :else false
   ))

(defn stars [n]
  (apply str (repeat n "*")))

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
  (let [names (fn [x] (:authors x))]
    (apply clojure.set/union (map names books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (str name (if birth-year
             (str " (" birth-year " - "
               (if death-year death-year "") ")")))))

(defn authors->string [authors]
  (apply str (interpose ", "  (map author->string authors))))

(defn book->string [book]
  (let [name (:title book)
        authors (authors->string (:authors book))]
    (str name ", written by " authors)))

(defn books->string [books]
  (let [books-as-strings (map book->string books)
        number-of-books (count books-as-strings)]
    (if (empty? books)
      (str "No books.")
      (str number-of-books
         (if (== number-of-books 1) " book. " " books. ")
         (apply str (interpose ", " books-as-strings))
         "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter
          (fn [author] (= (:name author) name))
          authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
