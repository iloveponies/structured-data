(ns structured-data)

(defn do-a-thing
  "(defn do-a-thing [x]
  (Math/pow (+ x x) (+ x x)))"
  [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
  )

(defn spiff
  "takes a vector and returns the sum of the first and third elements of the vector"
  [v]
  (+ (get v 0) (get v 2)))

(defn cutify
  " that takes a vector as a parameter and adds \"<3\" to its end"
  [v]
  (conj v "<3"))

(defn spiff-destructuring
  "Rewrite our earlier function spiff by destructuring its parameter. Call this new function"
  [v]
  (let [[first second third] v]
    (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width
  [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height
  [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square?
  "that returns true if rectangle is a square and otherwise false."
  [[[x1 y1] [x2 y2]]]
  (== (- x2 x1) (- y2 y1)))

(defn area
  "returns the area of the given rectangle."
  [[[x1 y1] [x2 y2]]]
  (* (- x2 x1) (- y2 y1))
  )

(defn contains-point?
  " returns true if rectangle contains point and otherwise false."
  [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle?
  "returns true if the rectangle inner is inside the rectangle outer and otherwise false."
  [outer [x y]]
  (and (contains-point? outer x) (contains-point? outer y)))

(defn title-length
  "counts the length of the book’s title."
  [book]
  (count (:title book)))

(defn author-count
  "that returns the amount of authors that book has."
  [book]
  (count (:authors book)))

(defn multiple-authors?
  "returns true if book has multiple authors, otherwise false."
  [book]
  (> (author-count book) 1))

(defn add-author
  "takes a book and an author as a parameter and adds author to books authors."
  [book new-author]
  (let [authors (:authors book)]
     (assoc book :authors (conj authors new-author))))

(defn alive?
  "takes an author map and returns true if the author is alive, otherwise false.
  An author is alive if the author does not have a death year."
  [author]
  (not (contains? author :death-year)))

(defn element-lengths
  "returns the lengths of every item in collection"
  [collection]
  (map count collection)
  )

(defn second-elements
  "takes a vector of vectors and returns a sequence of the second elements"
  [collection]
  (let [get-second (fn [collection] (get collection 1))]
    (map get-second collection)
   ))

(defn titles
  "takes a collection of books and returns their titles."
  [books]
  (map :title books))

(defn monotonic?
  "that returns true if a-seq is monotonic and otherwise false.
  A sequence is monotonic if is either inceasing or decreasing.
  In a decreasing sequence every element is at most as large as the previous one
  and in an increasing sequence every member is at least as large as the previous one."
  [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars
  "that returns a string with n aterisks. \\*."
  [n]
  (apply str (repeat n "*")))

(defn toggle
  "removes elem from a-set if a-set contains elem, and adds it to the set otherwise."
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates?
  "takes a sequence as a parameter and returns true if sequence contains some element multiple times. Otherwise it returns false."
  [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book
  "takes a book with the previous representation (authors in a vector) and returns the same book in the new representation (authors in a set)."
  [book]
  (assoc book :authors (set (:authors book))))

(defn has-author?
  "returns true if author is in the authors of book and otherwise false."
  [book author]
  (contains? (:authors book) author))

(defn authors
  "returns the authors of every book in books as a set."
  [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names
  "that works like the previous one and uses authors."
  [books]+
  (set (map :name (authors books))))

(defn author->string
  "returns a string representation of author."
  [author]
  (let [existence-date->string
        (fn [author]
          (cond
           (:death-year author) (str " (" (:birth-year author) " - " (:death-year author) ")")
           (:birth-year author) (str " (" (:birth-year author) " - )")
           :else ""))]
        (str (:name author) (existence-date->string author)))
  )

(defn authors->string
  "takes a sequence of authors as a parameter and returns a string representation of authors"
  [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string
  "takes a single book as a parameter and returns a string representation of book"
  [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string
  "that takes a sequence of books as a parameter and returns a string representation of books"
  [books]
  (let [books-count
        (fn [books]
        (cond
         (> (count books) 1) (str (count books) " books. ")
         (> (count books) 0) (str (count books) " book. ")
         :else (str "No books.")))

        print-dot
        (fn [books]
          (if (> (count books) 0)
            (str ".")))]

  (str (books-count books) (apply str (interpose ". " (map book->string books))) (print-dot books)))
  )

(defn books-by-author [author books]
  (let [book-has-author?
        (fn [book] (has-author? book author))]
    (filter book-has-author? books))
  )

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
