(ns structured-data)

;Ex1 Change the function do-a-thing so that it uses let to give a name to the common expression (+ x x) in its body.
(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    ))

;Ex2 Write the function (spiff v) that takes a vector and returns the sum of the first and third elements of the vector. What happens when you pass in a vector that is too short?
(defn spiff [v]
  (if (> (count v) 2)
    (+ (get v 0) (get v 2))
    nil
    )
  )

;Ex3 Write the function (cutify v) that takes a vector as a parameter and adds "<3" to its end.
(defn cutify [v]
  (conj v "<3")
  )

;Ex4 Rewrite our earlier function spiff by destructuring its parameter. Call this new function spiff-destructuring.
(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (if (and x y)
      (+ x y)
      nil
      )
    )
  )

(defn point [x y]
  [x y])

;Ex5 Write the functions (height rectangle) and (width rectangle) that return the height and width of the given rectangle. Use destructuring.
(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))
    )
  )

;Ex6 Write the function (square? rectangle) that returns true if rectangle is a square and otherwise false.
(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
  )

;Ex7 Write the function (area rectangle) that returns the area of the given rectangle.
(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

;Ex8 Write the function (contains-point? rectangle point) that returns true if rectangle contains point and otherwise false.
;Remember that you can give <= multiple parameters. (<= x y z) returns true if x≤y≤zx≤y≤z holds. Otherwise false.
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))
    )
  )

;Ex9 Write the function (contains-rectangle? outer inner) that returns true if the rectangle inner is inside the rectangle outer and otherwise false.
(defn contains-rectangle? [outer inner]
  (let [[inn_bottom-left inn_top-right] inner]
    (and
      (contains-point? outer inn_bottom-left)
      (contains-point? outer inn_top-right)
      )
    ))

;Ex10 Write the function (title-length book) that counts the length of the book’s title.
(defn title-length [book]
  (count (:title book))
  )

;Ex11 Write the function (author-count book) that returns the amount of authors that book has.
(defn author-count [book]
  (count (:authors book)))

;Ex12 Write the function (multiple-authors? book) that returns true if book has multiple authors, otherwise false
(defn multiple-authors? [book]
  (> (author-count book) 1))

;Ex13 Use assoc and conj to write the function (add-author book new-author) that takes a book and an author as a parameter and adds author to books authors.
(defn add-author [book new-author]
  (let [newAuthor (conj (:authors book) new-author)]
    (assoc book :authors newAuthor)
    )
  )

;Ex14 Write the function (alive? author) which takes an author map and returns true if the author is alive, otherwise false.
;An author is alive if the author does not have a death year.
(defn alive? [author]
  (not (contains? author :death-year)))

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
