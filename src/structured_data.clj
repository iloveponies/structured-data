(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


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

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and (contains-point? outer bl) (contains-point? outer tr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1) true false) )

(defn add-author [book new-author]
  (let [orig_authors (:authors book)
        new_authors (conj orig_authors new-author)
        new_book (assoc book :authors new_authors)]
    new_book))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (apply concat (repeat n "*"))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [orig_authors (:authors book)
        new_authors (set orig_authors)
        new_book (assoc book :authors new_authors)]
    new_book))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [book] (:authors book)) books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author_name (:name author)
        birth_year (:birth-year author)
        death_year (:death-year author)
        years (fn [by dy] (if (or by dy) (str " (" by " - " dy ")") ""))]
    (str author_name (years birth_year death_year))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
  (str title ", written by " authors)))

(defn books->string [books]
  (let [book_count (count books)
        disp_bc (fn [n]
            (cond
              (= n 0) "No books"
              (= n 1)  "1 book. "
              :else    (str n " books. ")))]

    (str (apply str (disp_bc book_count) (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
