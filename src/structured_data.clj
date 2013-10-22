(ns structured-data)

(defn do-a-thing [x]
  "Does a thing."
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  "Takes a vector and returns the sum of the first and third elements of the vector."
  (+ (get v 0)
     (get v 2)))


(defn cutify [v]
  "Takes a vector as a parameter and adds a heart to its end."
  (conj v "<3"))

(defn spiff-destructuring [v]
  "Rewrite our earlier function spiff by destructuring its parameter."
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  "Write the functions (height rectangle) and (width rectangle)
  that return the height and width of the given rectangle. Use destructuring."
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1][_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  "Checks if height and width are equal."
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  "Calculates area."
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  "Checks if a point resides within rectangle."
  (let [[px py]         point
       [[x1 y1][x2 y2]] rectangle]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  "Checks if a rectangle contains another rectangle (both endpoints!)"
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  "Returns a book's title length."
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [updated (conj (:authors book) new-author)]
    (assoc book :authors updated)))

(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
       (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  "removes elem from a-set if a-set contains elem, and adds it to the set otherwise."
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq)
           (count (set a-seq)))))

(defn old-book->new-book [book]
  "Converts the author sequence to a set."
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  "Checks if an author is listed for a book."
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string
  "Returns a string representation of an author."
  [author]
  (if (contains? author :birth-year)
        (let [years (str " ("
                     (:birth-year author)
                     " - "
                     (if (contains? author :death-year)
                       (:death-year author)
                       "")
                     ")")]
          (str (:name author) years))
        (:name author)))

(defn authors->string
  "Returns a list of authors separated by commas as a string."
  [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string
  "Returns a string representation of a book."
  [book]
  (str (:title book)
       ", written by " (authors->string (:authors book))))

(defn books->string [books]
  "Returns a string representation of a book collection."
  (cond
    (empty? books) "No books."
    (= 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
    :else (str (count books) " books. "
               (apply str (interpose ". " (map book->string books)))
               ".")))

(defn books-by-author
  "Find books by author from a collection."
  [author books]
  (let [found? (fn [x] (has-author? x author))]
         (filter found? books)))

(defn author-by-name
  "Find an author by name from a list of author objects."
  [name authors]
  (let [found? (fn [x] (= (:name x) name))]
    (first (filter found? authors))))

(defn living-authors
  "Filter living authors from a list of living/dead authors."
  [authors]
  (filter alive? authors))

(defn has-a-living-author?
  "Determine if a book has a living author."
  [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors
  "Show books with at least one living author from a list of books."
  [books]
  (filter has-a-living-author? books))
