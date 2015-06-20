(ns structured-data)

(defn
  do-a-thing
  "return with let statement the value"
  [x]
  (let [a (+ x x)]
    (Math/pow a a)))

(defn
  spiff
  "return sum of vector first and third element"
  [v]
  (let [a (get v 0)
        b (get v 2)]
  (+ a b)))

(defn
  cutify
  "conjuct <3 into the end of the vector"
  [v]
  (conj v "<3"))

(defn
  spiff-destructuring
  "spiff through desctruction"
  [v]
  (let [[x y z] v]
    (+ x z)))

(defn
  point
  "simply define point"
  [x y]
  [x y])

(defn
  rectangle
  "simply define rectangle two points"
  [bottom-left top-right]
  [bottom-left top-right])

(defn
  width
  "get rectangle width"
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x1 x2) -1)))

(defn
  height
  "get rectangle height"
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- y1 y2) -1)))

(defn
  square?
  "check if rectangle is square"
  [rectangle]
  (= (width rectangle) (height rectangle)))

(defn
  area
  "height times width and then you find the area of the rectangle"
  [rectangle]
  (* (width rectangle) (height rectangle)))

(defn
  contains-point?
  "check if rectangle contains given point"
  [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
      (and (<= x1 x3 x2) (<= y1 y3 y2)))))

(defn
  contains-rectangle?
  "check if rectangle contains inner rectangle"
  [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[x3 y3] [x4 y4]] inner]
      (and (and (<= x1 x3 x2) (<= x1 x4 x2))
           (and (<= y1 y3 y2) (<= y1 y4 y2))))))


(defn
  title-length
  "get book title length"
  [book]
  (count (get book :title)))

(defn
  author-count
  "count authors in the book"
  [book]
  (count (:authors book)))

(defn
  multiple-authors?
  "check if book contains more than one author"
  [book]
  (> (author-count book) 1))

(defn
  add-author
  "add author to book given"
  [book new-author]
  (assoc book :authors (conj  (get book :authors) new-author)))

(defn
  alive?
  "check wether author is alive"
  [author]
  (not (contains? author :death-year)))

(defn
  element-lengths
  "get element length"
  [collection]
  (map count collection))

(defn
  second-elements
  "returns second elements from given vector"
  [collection]
  (let [getsecond (fn [x] (get x 1))]
  (map getsecond collection)))

(defn
  titles
  "show books title"
  [books]
  (map :title books))

(defn
  monotonic?
  "check if monotonic"
  [a-seq]
  (cond
   (apply >= a-seq) true
   (apply <= a-seq) true
   :else false))

(defn
  stars
  "draw stars by the amount given"
  [n]
  (apply str (repeat n "*")))

(defn
  toggle
  "removes elem from a-set if a-set contains elem, and adds it to the set otherwise"
  [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn
  contains-duplicates?
  "check if contain duplicated elements"
  [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn
  old-book->new-book
  "converts books in vector to books in set"
  [book]
  (assoc book :authors (set (:authors book))))

(defn
  has-author?
  "check if book has given author"
  [book author]
  (let [author-name-in-parameter (fn [author] (:name author))]
    (let [all-author-names-in-book (fn [book] (set (map :name (:authors book))))]
      (contains? (all-author-names-in-book book) (author-name-in-parameter author)))))

(defn
  authors
  "return authors in every book as set"
  [books]
    (apply clojure.set/union (map :authors books) ))

(defn
  all-author-names
  "returns authors names in every book as set"
  [books]
  (set (map :name (authors books))))

(defn
  author->string
  "return string representation of author"
  [author]
  (cond
   (= (:birth-year author) nil) (:name author)
   :else (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")))

(defn
  authors->string
  "returns string representation of given sequence of authors"
  [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn
  book->string
  "return string representation of the given book"
  [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn
  books->string
  "return string representation of the given sequence of books"
  [books]
  (let [amountofbooks (fn [books] (count books))]
    (cond
      (= (amountofbooks books) 0) "No books."
      (= (amountofbooks books) 1) (apply str (str (amountofbooks books) " book. ") (apply str (interpose ". " (map book->string books))) ".")
     :else (apply str (str (amountofbooks books) " books. ") (apply str (interpose ". " (map book->string books))) "."))))

(defn
  books-by-author
  "return books by given author"
  [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn
  author-by-name
  "returns author by given name"
  [name authors] ; here problem
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn
  living-authors
  "returns sequence of authors that live still"
  [authors]
  (filter (fn [x] (alive? x)) authors))

(defn
  has-a-living-author?
  "check wether book has living authors"
  [book]
  (not (empty? (filter (fn [author] (not (contains? author :death-year))) (:authors book)))))

(defn
  books-by-living-authors
  "returns books with living authors"
  [books] ; here problem
  (filter (fn [book] (has-a-living-author? book)) books))


; %________%
