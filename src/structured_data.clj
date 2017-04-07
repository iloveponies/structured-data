(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        width (- x2 x1)
        height (- y2 y1)]
    (if (== width height) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
      (if (and (<= x1 x x2) (<= y1 y y2)) true false))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2])) true false)))

(defn title-length [book]
  (let [title (first book)]
    (count (second title))))

(defn author-count [book]
  (let [author (second book)]
    (count (second author))))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [title (get book :title)
        authors (get book :authors)
        updated-authors (conj authors new-author)]
    (assoc book :authors updated-authors)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [elem2 (fn [vect] (get vect 1))]
    (map elem2 collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply >= a-seq) (apply <= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq)) false true))

(defn old-book->new-book [book]
  (let [authors (get book :authors)
    updated-authors (set authors)]
    (assoc book :authors updated-authors)))

(defn has-author? [book author]
  (if (contains? (get book :authors) author) true false))

(defn authors [books]
  (let [authors
        (fn [book] (:authors book))]
    (set (apply clojure.set/union (map authors books)))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [name (:name author)
        years (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (if (contains? author :birth-year) (clojure.string/join [name " " years]) name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)
        authors-string (authors->string authors)]
    (clojure.string/join [title ", written by " authors-string])))

(defn books->string [books]
  (let [book-count (count books)
        books-strings (apply str (interpose ". "(map book->string books)))]
    (cond
      (= book-count 0) "No books."
      (= book-count 1) (clojure.string/join [book-count " book. " books-strings "."])
      :else (clojure.string/join [book-count " books. " books-strings "."]))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (if (empty? (living-authors authors)) false true)))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
