(ns structured-data)

(defn do-a-thing [x]
  (let [doubleX (+ x x)]
  (Math/pow doubleX doubleX)))

(defn spiff [v]
  (cond
   (> (count v) 2) (+ (get v 0) (get v 2))
   :else "?"))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (cond
   (> (count v) 2)
     (let [[a b c] v ] (+ a c))
   :else "?"))

(defn point [x y]
  [x y])

(defn abs [x]
  (cond
   (>= x 0) x
   :else (- x)))

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
  (abs (- y1 y2))))

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
  (abs (- x1 x2))))

(defn square? [rectangle]
  (= (width rectangle)(height rectangle)))

(defn area [rectangle]
  (* (height rectangle)(width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle]
    (let [[pointX pointY] point]
      (and (<= x1 pointX x2) (<= y1 pointY y2)))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1)
         (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authorsSoFar (:authors book)
        authorsNow (conj authorsSoFar new-author)]
       (assoc book :authors authorsNow)))

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
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq)(count (set a-seq))))

(defn old-book->new-book [book]
  (let [authorsInSet (set (:authors book))]
    (assoc book :authors authorsInSet)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authorsInOneBook
        (fn [book] (:authors book))]
    (apply clojure.set/union (map authorsInOneBook books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (cond
     (contains? author :birth-year) (str name " " years)
     :else name)))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [booksInString (apply str(interpose ". " (map book->string books)))]
    (cond
     (empty? books) "No books."
     (= 1 (count books)) (str "1 book. " booksInString ".")
     :else (str (count books) " books. " booksInString "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [authorsFiltered
        (filter (fn [x] (= name (:name x))) authors)]
    (if (empty? authorsFiltered)
      nil
      (first authorsFiltered))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
