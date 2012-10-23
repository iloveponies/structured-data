(ns structured-data)

(defn do-a-thing 
  "Does a thing."
  [x]
  (let [sumx (+ x x)]
    (Math/pow sumx sumx)))

(defn spiff 
  "Returns the sum of the first and third elements of a vector."
  [v]
  (+ (get v 0) (get v 2)))

(defn cutify
  "Adds love to a vector."
  [v]
  (conj v "<3"))

(defn spiff-destructuring 
  "Spiff by destructuring."
  [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width 
  "Returns the width of a rectangle."
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height
  "Returns the height of a rectangle."
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square?
  "Checks if a regtangle is square."
  [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area
  "Calculates area of a rectangle."
  [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? 
  "Determine if a point resides inside a rectangle."
  [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle?
  "Determine if a rectangle resides inside a rectangle."
  [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length 
  "Returns the length of a book title."
  [book]
  (count (get book :title)))

(defn author-count
  "Returns the number of authors for a book."
  [book]
  (count (get book :authors)))

(defn multiple-authors? 
  "Determine if a book has multiple authors."
  [book]
  (> (author-count book) 1))

(defn add-author 
  "Add an author to a book."
  [book new-author]
  (assoc book :authors 
    (conj (:authors book) new-author)))

(defn alive? 
  "Checks if author is still alive (does not have a death year!)"
  [author]
  (not (contains? author :death-year)))

(defn element-lengths
  "Returns the length of every item in collection."
  [collection]
  (map count collection))

(defn second-elements 
  "Returns the second element of every item in collection."
  [collection]
  (map second collection))

(defn titles 
  "Returns the titles of a list of books."
  [books]
  (map :title books))

(defn monotonic? 
  "Determines if a sequence is monotonic (increasing or decreasing)."
  [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars 
  "Returns n stars as a string."
  [n]
  (apply str (repeat n "*")))

(defn toggle 
  "Removes elem from a-set if exists, otherwise adds it to the set."
  [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? 
  "Determine if a sequence contains duplicates by comparing lengths."
  [a-seq]
  (not= (count a-seq)
        (count (set a-seq))))

(defn old-book->new-book 
  "Converts a book to list authors as a set instead of a vector."
  [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? 
  "Determines if a book has a given author."
  [book author]
  (contains? (:authors book) author))

(defn authors 
  "Returns all authors of every book in books."
  [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names
  "Returns all author names of every book in books."
  [books]  
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

; %________%
