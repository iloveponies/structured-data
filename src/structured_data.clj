(ns structured-data)

(defn do-a-thing [x]
  ( let [xx (+ x x)](
    Math/pow xx xx)))

(do-a-thing 2)

(defn spiff [v]
  (+ (get v 2) (get v 0))
)

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
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (= (- x1 x2)(- y1 y2))
))

(square? (rectangle [3 2] [1 1]))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (* (- x2 x1) (- y2 y1))
))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
  (and (<= x1 px x2) (<= y1 py y2))
))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
       [[in_x1 in_y1] [in_x2 in_y2]] inner]
  (and
    (contains-point? outer [in_x1 in_y1])
    (contains-point? outer [in_x2 in_y2])
)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
 (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  ( let [authors (:authors book)]
    (assoc book :authors (conj authors new-author)))
)

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [length (fn [x] (count x))]
    (map length collection)))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
 (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
 (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author) b_year (:birth-year author) d_year (:death-year author)]
    (cond
    (nil? b_year) name
    (nil? d_year) (str name " (" b_year " - )")
    :else (str name " (" b_year " - " d_year ")" ))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [authors (authors->string (:authors book))]
    (str (:title book) ", written by " authors)))

(defn books->string [books]
  (let [ num_books (count books)
         book_list (map book->string books)
         books_str (apply str (interpose ". " book_list))]
    (cond
    (= num_books 0) "No books."
    (= num_books 1) (str "1 book. " books_str ".")
    :else (str num_books " books. " books_str "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (living-authors (:authors book))]
  (not (empty? authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
