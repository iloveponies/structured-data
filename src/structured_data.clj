(ns structured-data)

(defn do-a-thing 
  "Returns the result of (x+x)^(x+x)." 
  [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff 
  "Returns the sum of first and 3rd elementh of a given vector v." 
  [v]
  (+ (get v 0) (get v 2)))

(defn cutify 
  "Cutifies the vector v by appending '<3' to the end of the vector." 
  [v]
  (conj v "<3"))

(defn spiff-destructuring 
  "Same as spiff, but with destructuring the vector." 
  [v]
  (let [[first second third] v]
    (+ first third)))

(defn point 
  "Defines a point as a vector of two elements." 
  [x y]
  [x y])

(defn rectangle 
  "Defines a rectangle with bottom-left and top-right coordinates of type point." 
  [bottom-left top-right]
  [bottom-left top-right])

(defn width 
  "Returns the width of a rectangle." 
  [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))

(defn height 
  "Returns the height of a rectangle." 
  [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? 
  "Returns true, if the rectangle is a square, otherwise false." 
  [rectangle]
  (if (== (width rectangle) (height rectangle)) 
    true 
    false))

(defn area 
  "Returns the area of the rectangle." 
  [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? 
  "Returns true, if the rectangle contains the point, otherwise false." 
  [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
       [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? 
  "Returns true, if the outer rectangle contains the inner rectangle, otherwise false." 
  [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

(defn title-length 
  "Returns the title length of a book." 
  [book]
  (count (:title book)))

(defn author-count 
  "Returns the number of authors of a book." 
  [book]
  (count (:authors book)))

(defn multiple-authors? 
  "Returns true, if book has multiple authors, otherwise false." 
  [book]
  (if (> (author-count book) 1) 
    true 
    false))

(defn add-author 
  "Adds a new author to the book." 
  [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? 
  "Returns true, if the author is alive." 
  [author]
  (not (contains? author :death-year)))

(defn element-lengths 
  "Returns elementh lengths in a given collection." 
  [collection]
  (let [element-length (fn [x] (count x))]
    (map element-length collection)))

(defn second-elements 
  "Returns 2nd elements in a given collection." 
  [collection]
  (let [seconds (fn [x] (get x 1))]
    (map seconds collection)))

(defn titles 
  "Returns all book titles in a given collection." 
  [books]
  (map :title books))

(defn monotonic? 
  "Returns true if given collection is monotonic, otherwise false." 
  [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars 
  "Returns a string with n asterisks." 
  [n]
  (apply str (repeat n "*")))

(defn toggle 
  "If element exists in a set, removes it. Otherwise adds it." 
  [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem) 
    (conj a-set elem)))

(defn contains-duplicates? 
  "Returns true if the sequence contains duplicates, otherwise false." 
  [a-seq]
  (let [a-set (set a-seq)]
    (not (== (count a-set) (count a-seq)))))

(defn old-book->new-book 
  "Returns book in a new format, i.e. authors vector changed to a set." 
  [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? 
  "Returns true if the author is one of the book authors." 
  [book author]
  (contains? (get book :authors) author))

(defn authors 
  "Returns authors of every book in a set." 
  [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names 
  "Returns names of the authors of the books." 
  [books]
  (set (map :name (authors books))))

(defn author->string 
  "Returns the author as a string." 
  [author]
  (let [name (get author :name)
        years (cond 
                (contains? author :death-year) (str " (" (get author :birth-year) " - " (get author :death-year) ")")
                (contains? author :birth-year) (str " (" (get author :birth-year) " - )")
                :else "")]
    (str name years)))

(defn authors->string 
  "Returns the authors as a string." 
  [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string 
  "Returns a string representation of a book." 
  [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string 
  "Returns a string representation of a collection of books." 
  [books]
  (cond
    (== 0 (count books)) "No books."
    (== 1 (count books)) (str "1 book. " (book->string (first books)) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author 
  "Returns books by given author." 
  [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name 
  "Searches author by name." 
  [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors 
  "Returns living authors." 
  [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? 
  "Returns true if book has a living author, otherwise false." 
  [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors 
  "Returns books that have a living author." 
  [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
