(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (>= (count v) 3)
    (+ (get v 0) (get v 2))
    nil
    ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (>= (count v) 3)
    (let [[x y z] v] (+ x z))
    nil))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle
         [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
  (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
  (map second-element collection)))

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
  (let [num_elementi (count a-seq)
        num_elem_set (count (set a-seq))]
  (not (= num_elementi num_elem_set))))

(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))]
  (assoc book :authors authors-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        str_year (if birth-year (str " (" birth-year " - " death-year ")") "")]
  (str name str_year)))

(defn authors->string [authors]
  (let [authors_string (interpose ", " (map author->string authors))]
  (apply str authors_string)))

(defn book->string [book]
  (let [authors (authors->string (:authors book))]
  (str (:title book) ", written by " authors)))

(defn books->string [books]
  (let [book_count (count books)
        books_string (str (apply str (interpose ". " (map book->string books))) ".")]
    (cond
     (= book_count 0) "No books."
     (= book_count 1) (str "1 book. " books_string)
     :else (str book_count " books. " books_string ))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author))  books))

(defn author-by-name [name authors]
  (let [authors_filtered (filter (fn [x] (= (:name x) name)) authors)]
  (first authors_filtered)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (boolean (seq (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter  has-a-living-author? books))

; %________%
