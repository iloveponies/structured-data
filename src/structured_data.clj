(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[v0 v1 v2]]
  (+ v0 v2))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x0 y0] [x1 y1]]]
  (- x1 x0))

(defn height [[[x0 y0] [x1 y1]]]
  (- y1 y0))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x0 y0] [x1 y1]] [x y]]
  (and (>= x1 x x0) (>= y1 y y0)))

(defn contains-rectangle? [outer [bottom-left top-right]]
  (and (contains-point? outer bottom-left)
       (contains-point? outer top-right)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [
    years (if (contains? author :birth-year)
      (str " ("
        (get author :birth-year) " - " (get author :death-year)
        ")"))
    ]
    (str (get author :name) years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by "
       (authors->string (get book :authors))))

(defn books->string [books]
  (let [
      books-count (count books)
      books-count-str (if (= books-count 1) "1 book"
        (str books-count " books"))
      books-str (apply str (interpose " " (map (fn [book]
        (str (book->string book) ".")) books)))
    ]
    (str
      (if (empty? books) "No books."
        (str books-count-str ". " books-str)))))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (get book :authors) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (get author :name) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (not (contains? author :death-year))) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
