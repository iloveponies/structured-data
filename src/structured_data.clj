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

(defn alive? [author]
  :-)

(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
