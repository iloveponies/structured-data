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

;Ex15 Write the function (element-lengths collection) that returns the lengths of every item in collection.
(defn element-lengths [collection]
  (map count collection)
  )


;Ex16 Use map to write the function (second-elements collection) that takes a vector of vectors and returns a sequence of the second elements.
;Remember that you can use get to index a vector.
;Use fn and let to create a helper function and use it with map.
(defn second-elements [collection]
  (let [sec (fn [x] (second x))]
    (map sec collection)
    )
  )

;Ex17 Write the function (titles books) that takes a collection of books and returns their titles.
(defn titles [books]
  (map :title books))


;Ex18 Write the function (stars n) that returns a string with n aterisks \*.
(defn stars [n]
  (apply str (repeat n "*")))


;Ex19 Write the function (monotonic? a-seq) that returns true if a-seq is monotonic and otherwise false.
;A sequence is monotonic if is either inceasing or decreasing. In a decreasing sequence every element is at most as large as the previous one and in an increasing sequence every member is at least as large as the previous one.
(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)
    )
  )

;Ex20 Write the function (toggle a-set elem) that removes elem from a-set if a-set contains elem, and adds it to the set otherwise.
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

;Ex21 Write the function (contains-duplicates? sequence) that takes a sequence as a parameter and returns true if sequence contains some element multiple times. Otherwise it returns false.
(defn contains-duplicates? [a-seq]
  (let [unique (set a-seq)]
    (not= (count unique) (count a-seq))
    )
  )

;Ex22 Write the function (old-book->new-book book) that takes a book with the previous representation (authors in a vector) and returns the same book in the new representation (authors in a set).
;Use assoc to change the representation. Do not construct a new map using the map literal syntax.
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )


;Ex23 Write the function (has-author? book author) that returns true if author is in the authors of book and otherwise false.
(defn has-author? [book author]
  (contains? (:authors book) author)
  )

;Ex24 Write the function (authors books) that returns the authors of every book in books as a set.
(defn authors [books]
  (let [author (fn [book] (:authors book))]
    (apply clojure.set/union (map author books))
    )
  )

;Ex25 Write the function (all-author-names books) that works like the previous one and uses authors.
(defn all-author-names [books]
  (set (map :name (authors books)))
  )

;Ex26 Write the function (author->string author) that returns a string representation of author as follows:
;You can assume that every author with a :death-year also has a :birth-year.
(defn author->string [author]
  (let [name (:name author)
        year (if (contains? author :birth-year)
               (str " (" (:birth-year author) " - " (:death-year author) ")")
               )]
    (str name year)
    )
  )

;Ex27 Write the function (authors->string authors) which takes a sequence of authors as a parameter and returns a string representation of authors
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

;Ex28 Write the function (book->string book) takes a single book as a parameter and returns a string representation of book
(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)
    )
  )

;Ex29 Write the function (books->string books) that takes a sequence of books as a parameter and returns a string representation of books
(defn books->string [books]
  (let [cnt (count books)
        titles (set (map book->string books))
        titlesSet (apply str (interpose ". " titles))]
    (str
      (case cnt
        0 "No books"
        1 "1 book. "
        (str cnt " books. ")
        ) titlesSet ".")
    )
  )


;Ex30 Write the function (books-by-author author books).
(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

;Ex31 Write the function (author-by-name name authors) that takes a string name and a sequence of authors and returns an author with the given name if one is found.
;If one is not found, then nil should be returned.
(defn author-by-name [name authors]
  (let [findAuthor (fn [author] (= (:name author) name))]
    (first (set (filter findAuthor authors)))
    )
  )

;Ex32 Write the function (living-authors authors) that takes a sequence of authors and returns those that are alive. Remember alive?.
(defn living-authors [authors]
  (filter alive? authors))

;Ex33 Write the function (has-a-living-author? book) that returns true if book has a living author, and otherwise false.
(defn has-a-living-author? [book]
  (not (empty? (living-authors (authors [book]))))
  )

;Ex34 Write the function (books-by-living-authors books) that takes a sequence of books as a parameter and returns those that have a living author.
(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

