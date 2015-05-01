(ns structured-data)

(defn do-a-thing [x]
  (let [x_two_times (+ x x)]
    (Math/pow x_two_times x_two_times)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x  z)))

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
  (== (width rectangle)  (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)  (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (if (and (<= x1 x3 x4 x2) (<= y1 y3 y4 y2)) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (count (:authors book))) true false))

(defn add-author [book new-author]
  (let [original book
    new      (assoc original :authors (conj (:authors book) new-author))]
    new))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secondele (fn [x] (get x 1))]
    (map secondele collection)))

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
  (if (< (count (set a-seq)) (count a-seq)) true false))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

(defn authors [books]
    (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [author_name (:name author)
        birth_year (:birth-year author)
        death_year (:death-year author)]
    ( if (not birth_year)
     (apply str [author_name])
      (apply str [author_name " (" birth_year " - " death_year ")"]))))

(defn authors->string [authors]
  (let [author-strings
         (map author->string authors)]
    (apply str (interpose ", " author-strings))))

(defn book->string [book]
  (let [book_name (:title book)
        authors_string (authors->string (:authors book))]
    (apply str [book_name  ", written by " authors_string])))

(defn books->string [books]
  (let [num (count books)]
    (if (== num 0)
      "No books."
      (if (== num 1)
        (apply str ["1 book. " (book->string (get books 0)) "."])
        (apply str [num " books. " (apply str (interpose ". " (map book->string books))) "."])))))

(defn books-by-author [author books]
  (let [has_author
         (fn [book] (has-author? book author))]
    (filter has_author books)))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
