(ns structured-data)


;; Ex 1
;; The following function does a thing:
;; Change the function do-a-thing so that it uses let to give a name to the common expression (+ x x) in its body.

(defn do-a-thing [x]
  (let [dublex (+ x x)]
   (Math/pow dublex dublex)))


;; Ex 2
;; Write the function (spiff v) that takes a vector and returns the sum of the first and third elements of the vector. What happens when you pass in a vector that is too short?

(defn spiff [v]
  (+ (get v 0)  (get v 2)))


;; Ex 3
;; Write the function (cutify v) that takes a vector as a parameter and adds "<3" to its end.

(defn cutify [v]
  (conj v "<3"))


;; Ex 4
;; Rewrite our earlier function spiff by destructuring its parameter. Call this new function spiff-destructuring.

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))


(defn point [x y]
  [x y])


(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


;; Ex 5
;; Write the functions (height rectangle) and (width rectangle) that return the height and width of the given rectangle. Use destructuring.

(defn height [rectangle]
  (let [ [[_ y1] [_ y2]] rectangle]
  (- y2 y1)) )

(defn width [rectangle]
  (let [ [[x1 _] [x2 _]] rectangle]
  (- x2 x1)) )


;; Ex 6
;; Write the function (square? rectangle) that returns true if rectangle is a square and otherwise false.

(defn square? [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
  (= (height rectangle) (width rectangle)) ))


;; Ex 7
;; Write the function (area rectangle) that returns the area of the given rectangle.

(defn area [rectangle]
  (* (height rectangle) (width rectangle)) )


;; Ex 8
;; Write the function (contains-point? rectangle point) that returns true if rectangle contains point and otherwise false.
;; Remember that you can give <= multiple parameters. (<= x y z) returns true if x≤y≤zx≤y≤z holds. Otherwise false.
;; Hint: and is useful.
;; use destructuring.

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2]] rectangle
         [px py]           point]
    (and (<= x1 px x2) (<= y1 py y2))))


;; Ex 9
;; Write the function (contains-rectangle? outer inner) that returns true if the rectangle inner is inside the rectangle outer and otherwise false.
;; Hint: use contains-point?

(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2) )))


;; Ex 10
;; Write the function (title-length book) that counts the length of the book’s title.

(defn title-length [book]
  (count (book :title)))


;; Ex 11
;; Write the function (author-count book) that returns the amount of authors that book has.

(defn author-count [book]
  (count (book :authors)))


;; Ex 12
;; Write the function (multiple-authors? book) that returns true if book has multiple authors, otherwise false.

(defn multiple-authors? [book]
  (< 1 (count (book :authors))))


;; Ex 13
;; Use assoc and conj to write the function (add-author book new-author) that takes a book and an author as a parameter and adds author to books authors.
;; Hint: use let to avoid pain

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))


;; Ex 14
;; Write the function (alive? author) which takes an author map and returns true if the author is alive, otherwise false.
;; An author is alive if the author does not have a death year.

(defn alive? [author]
  (not (contains? author :death-year)))


;; Ex 15
;; Write the function (element-lengths collection) that returns the lengths of every item in collection.

(defn element-lengths [collection]
  (map count collection))


;; Ex 16
;; Use map to write the function (second-elements collection) that takes a vector of vectors and returns a sequence of the second elements.
;; Remember that you can use get to index a vector.
;; Use fn and let to create a helper function and use it with map.

(defn second-elements [collection]
  (let [scnd (fn[x] (get x 1))]
  (map scnd collection)))


;; Ex 17
;; Write the function (titles books) that takes a collection of books and returns their titles.
;; Using our earlier examples:

(defn titles [books]
  (map :title books))


;; Ex 18
;; Write the function (stars n) that returns a string with n aterisks \*.
;; The function (repeat n x) returns a sequence with n xs:

(defn stars [n]
  (apply str (repeat n "*")))


;; Ex 19
;; Write the function (monotonic? a-seq) that returns true if a-seq is monotonic and otherwise false.
;; A sequence is monotonic if is either inceasing or decreasing. In a decreasing sequence every element is at most as large as the previous one and in an increasing sequence every member is at least as large as the previous one.
;; Use apply.
;; Hint: <= might be useful

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))


;; Ex 20
;; Write the function (toggle a-set elem) that removes elem from a-set if a-set contains elem, and adds it to the set otherwise.

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


;; Ex 21
;; Write the function (contains-duplicates? sequence) that takes a sequence as a parameter and returns true if sequence contains some element multiple times. Otherwise it returns false.

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))


;; Ex 22
;; Write the function (old-book->new-book book) that takes a book with the previous representation (authors in a vector) and returns the same book in the new representation (authors in a set).
;; Use assoc to change the representation. Do not construct a new map using the map literal syntax.

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))


;; Ex 23
;; Write the function (has-author? book author) that returns true if author is in the authors of book and otherwise false.

(defn has-author? [book author]
  (contains? (book :authors) author))


;; Ex 24
;; Write the function (authors books) that returns the authors of every book in books as a set.

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


;; Ex 25
;; Write the function (all-author-names books) that works like the previous one and uses authors.

(defn all-author-names [books]
  (set (map :name (authors books))))


;; Ex 26
;; Write the function (author->string author) that returns a string representation of author as follows:
;; You can assume that every author with a :death-year also has a :birth-year.

(defn author->string [author]
  (str (:name author)
       (if (:birth-year author)
         (str " ("
              (:birth-year author)
              " - "
              (:death-year author)
              ")"))))


;; Ex 27
;; Write the function (authors->string authors) which takes a sequence of authors as a parameter and returns a string representation of authors in the following manner:

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors) )))


;; Ex 28
;; Write the function (book->string book) takes a single book as a parameter and returns a string representation of book as follows:

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


;; Ex 29
;; Write the function (books->string books) that takes a sequence of books as a parameter and returns a string representation of books like this:

(defn books->string [books]
  (cond
    (= 0 (count books)) "No books."
    :else (apply str (count books)
                     (if (= 1 (count books)) " book. " " books. ")
                     (apply str (interpose ". " (map book->string books)))
                     ".")))


;; Ex 30
;; Write the function (books-by-author author books).
;; Hint: has-author?

(defn books-by-author [author books]
  (filter #(contains? (%1 :authors) author) books))


;; Ex 31
;; Write the function (author-by-name name authors) that takes a string name and a sequence of authors and returns an author with the given name if one is found. If one is not found, then nil should be returned.
;; Hint: remember first

(defn author-by-name [name authors]
  (first (filter #(= (%1 :name) name) authors)))


;; Ex 32
;; Write the function (living-authors authors) that takes a sequence of authors and returns those that are alive. Remember alive?.

(defn living-authors [authors]
  (filter #(alive? %1) authors))


;; Ex 33
;; Write the function (has-a-living-author? book) that returns true if book has a living author, and otherwise false.

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))


;; Ex 34
;; Write the function (books-by-living-authors books) that takes a sequence of books as a parameter and returns those that have a living author.

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %1) books))


; %________%
