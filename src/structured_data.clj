(ns structured-data)

(defn
  do-a-thing
  "First, doubles the input value. Then, returns the power of the result value by itself."
  [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn
  spiff
  "Returns the sum of the first and third index of the input vector."
  [v]
  (let [xx (get v 0)
        yy (get v 2)]
    (+ xx yy)))

(defn
  cutify
  "Adds a '<3' to the end of the input vector."
  [v]
  (let [xx "<3"]
    (conj v xx)))

(defn
  spiff-destructuring [v]
  "Returns the sum of the first and third index of the input vector."
  [v]
  (let [[x _ y] v]
    (+ x y)))

(defn
  point
  "Representation for a simple point with x and y values."
  [x y]
  [x y])

(defn
  rectangle
  "Representation of a rectangle, which takes two points as an input."
  [bottom-left top-right]
  [bottom-left top-right])

(defn
  width
  "Calculates the width of the rectangle."
  [rectangle]
  (let [[[^int x1 _] [^int x2 _]] rectangle]
    (Math/abs (- x1 x2))))

(defn
  height
  "Calculates the height of the rectangle."
  [rectangle]
  (let [[[_ ^int y1] [_ ^int y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn
  square?
  "Returns true if the rectangle is a square."
  [rectangle]
  (let [xx (width rectangle)
        yy (height rectangle)]
    (if (== xx yy)
      true
      false)))

(defn
  area
  "Returns the area of input rectangle."
  [rectangle]
  (let [xx (width rectangle)
        yy (height rectangle)]
    (* xx yy)))

(defn
  contains-point?
  "Returns true if the rectangle contains the input point."
  [rectangle point]
  (let [ [[^int x1 y1] [^int x2 y2]] rectangle
         [^int xx yy] point ]
    (if (and (<= x1 xx x2) (<= y1 yy y2))
      true
      false)))

(defn
  contains-rectangle?
  "Returns true if the inner rectangle is inside the outer rectangle."
  [outer inner]
  (let [ p1 (get inner 0)
        p2 (get inner 1)
        xx (contains-point? outer p1)
        yy (contains-point? outer p2) ]
    (if (and xx yy)
      true
      false)))

(defn
  title-length
  "Returns the length of the books title."
  [book]
  (let [title (get book :title)]
    (count title)))

(defn
  author-count
  "Returns the amount of authors for the book."
  [book]
  (let [authors (get book :authors)]
    (count authors)))

(defn
  multiple-authors?
  "Returns true if the book has multiple authors."
  [book]
  (let [author-amount (author-count book)]
    (if (> author-amount 1)
      true
      false)))

(defn
  add-author
  "Adds a new author to the input book."
  [book new-author]
  (let [authors (get book :authors)]
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
