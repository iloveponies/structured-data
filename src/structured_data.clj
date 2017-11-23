(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [sz (count v)]
    (cond
      (>= sz 3) (+ (get v 0) (get v 2))
      (>= sz 1) (get v 0)
      :else nil)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x0 x1 x2]]
  (cond
    (not (= x2 nil)) (+ x0 x2)
    (not (= x0 nil)) (x0)
    :else nil))

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
  (let [[bottom-left up-right] rectangle]
    (let [w (width [bottom-left up-right])
          h (height [bottom-left up-right])]
      (if (= h w)
        true
        false))))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x0 y0] point]
    (if (and (<= x1 x0 x2) (<= y1 y0 y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[inner-lower-left inner-up-right] inner]
    (if (and (contains-point? outer inner-lower-left)
             (contains-point? outer inner-up-right))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second
        (fn [x] 
          (let [[x1 x2] x]
            x2))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false)) 

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [nr (count a-seq)]
    (let [nr-unique (count (set a-seq))]
      (if (> nr nr-unique)
        true
        false))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors-book
        (fn [book]
          (:authors book))]
    (apply clojure.set/union (map authors-book books))))

(defn all-author-names [books]
  (set  (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)]
    (let [birth-year (:birth-year author)]
      (let [death-year (:death-year author)]
        (cond
          (and birth-year death-year) (str author-name " (" birth-year " - " death-year \))
          birth-year (str author-name " (" birth-year " - )")
          :else (str author-name)
          )))))

(defn authors->string [authors]
  (let [print-author
        (fn [author] (author->string author))]
    (apply str (interpose ", " (map print-author authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (= 0 (count books)) (str "No books.")
    (= 1 (count books)) (str "1 book. " (apply book->string books) ".")
    :else (let [each-book
                (fn [book] (book->string book))]
            (apply str (count books) " books. " (apply str (map each-book books)) "."))))
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors-book
        (fn [book]
          (:authors book))]
    (apply clojure.set/union (map authors-book books))))

(defn all-author-names [books]
  (set  (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)]
    (let [birth-year (:birth-year author)]
      (let [death-year (:death-year author)]
        (cond
          (and birth-year death-year) (str author-name " (" birth-year " - " death-year \))
          birth-year (str author-name " (" birth-year " - )")
          :else (str author-name)
          )))))

(defn authors->string [authors]
  (let [print-author
        (fn [author] (author->string author))]
    (apply str (interpose ", " (map print-author authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (= 0 (count books)) (str "No books.")
    (= 1 (count books)) (str "1 book. " (apply book->string books) ".")
    :else (let [each-book
                (fn [book] (book->string book))]
            (apply str (count books) " books. " (apply str (map each-book books)) "."))))
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors-book
        (fn [book]
          (:authors book))]
    (apply clojure.set/union (map authors-book books))))

(defn all-author-names [books]
  (set  (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)]
    (let [birth-year (:birth-year author)]
      (let [death-year (:death-year author)]
        (cond
          (and birth-year death-year) (str author-name " (" birth-year " - " death-year \))
          birth-year (str author-name " (" birth-year " - )")
          :else (str author-name)
          )))))

(defn authors->string [authors]
  (let [print-author
        (fn [author] (author->string author))]
    (apply str (interpose ", " (map print-author authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (= 0 (count books)) (str "No books.")
    (= 1 (count books)) (str "1 book. " (apply book->string books) ".")
    :else (let [each-book
                (fn [book] (book->string book))]
            (str (count books) " books. " (apply str (interpose ". " (map each-book books))) "."))))

(defn books-by-author [author books]
  (filter (fn  [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-author [author books]
  (filter (fn  [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-author [author books]
  (filter (fn  [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
