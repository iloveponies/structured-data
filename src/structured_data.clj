(ns structured-data)

(defn do-a-thing [x]
  (let [xx  (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[pointX pointY] point
       [[x1 y1] [x2 y2]] rectangle] (and (<= x1 pointX x2) (<= y1 pointY y2))))

(defn contains-rectangle? [outer inner]
  (let [[pointX pointY] inner]
    (and (contains-point? outer pointX) (contains-point? outer pointY))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [a (:authors book)] (assoc book :authors (conj a new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn count-length [item]
  (count item))

(defn element-lengths [collection]
  (map count-length collection))

(defn second-elements [collection]
  (let [s (fn [v] (get v 1))] (map s collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [x (fn [v] (apply <= v))
         y (fn [v] (apply >= v))]
    (or (x a-seq)(y a-seq))))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(= (count a-seq)(count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
   (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [name (:name author) years
        (if (contains? author :death-year)
          (str " (" (:birth-year author) " - " (:death-year author) ")")
            (if (contains? author :birth-year) (str " (" (:birth-year author) " - )")
              "" ))]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [amount
        (cond
           (empty? books) "No books."
           (= (count books) 1) "1 book. "
           :else (str (count books) " books. "))
        stringofbooks (apply str(interpose ", " (map book->string books)))
        period (if (empty? books) "" ".")]
    (str amount stringofbooks period)))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (if (empty? authors) nil (if (= name (:name (first authors))) (first authors) (author-by-name name (rest authors)))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
   (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
