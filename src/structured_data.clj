(ns structured-data)

(defn do-a-thing [x]
  (let [doubled (fn [x] (+ x x))]
    (Math/pow (doubled x) (doubled x))))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x0] [x1]] rectangle]
    (- x1 x0)))

(defn height [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- y1 y0)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x0 y0] [x1 y1]] rectangle
        [px py] point]
    (and
     (<= x0 px x1)
     (<= y0 py y1))))

(defn contains-rectangle? [outer inner]
  (let [[[x0 y0] [x1 y1]] inner]
    (and
     (contains-point? outer (point x0 y0))
     (contains-point? outer (point x1 y1)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (boolean (:death-year author))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [f (fn [a] (get a 1))]
    (map f collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [birth-year (:birth-year author)
        death-year (:death-year author)]
    (str (:name author)
         (if (some boolean [birth-year death-year])
           (str " (" birth-year " - " death-year ")")
           ""))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (cond (empty? books) "No books."
        (= 1 (count books)) (str "1 book. " (book->string (first books)) ".")
        :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))



(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (let [results (filter (fn [author] (= (:name author) name)) authors)]
    (if (> (count results) 0)
      (first results)
      nil)))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (some alive? (:authors book))
    true
    false))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%



