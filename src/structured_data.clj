(ns structured-data)

(defn 
  do-a-thing 
  "does something"
  [x]
  (let  [twox  (+ x x)]
    (Math/pow twox twox)))

(defn 
  spiff 
  "return the sum of the first and third element of a vector"
  [v]
  (+ (get v 0) (get v 2)))

(defn 
  cutify 
  "adds a cute \"<3\" to the end of the parameter vector"
  [v]
  (conj v "<3"))

(defn 
  spiff-destructuring
  "return the sum of the first and third element of a vector, using destructoring"
  [v]
  (let [[x _ y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn 
  width 
  "return the width of parameter rectangle, which consists of a coordinate pair"
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn 
  height 
  "return the height of parameter rectangle, which consists of a coordinate pair"
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn 
  square? 
  "is parameter rectangle a square?"
  [rectangle]
  (= (width rectangle) (height rectangle)))

(defn 
  area
  "return the area of parameter rectangle" 
  [rectangle]
  (* (width rectangle) (height rectangle)))

(defn 
  contains-point?
  "is parameter point within rectangle borders?"
  ;
  ; assuming that the parameter rectangle has the bottom left corner defined first
  ;
  [rectangle point]
  (let 
    [[[rx1 ry1] [rx2 ry2]] rectangle
    [px py] point] 
    (and (<= rx1 px rx2) (<= ry1 py ry2))))

(defn 
  contains-rectangle? 
  "is parameter inner rectangle contained within parameter outer rectangle?"
  ;
  ; same assumptions as in contains-point
  ;
  [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) 
         (contains-point? outer top-right))))

(defn 
  title-length 
  "returns the character length of parameter book title"
  [book]
  (count (:title book)))

(defn 
  author-count 
  "return the number of authors on parameter book"
  [book]
  (count (:authors book)))

(defn 
  multiple-authors? 
  "does parameter book have multiple authors?"
  [book]
  (< 1 (author-count book)))

(defn 
  add-author 
  "adds new-author to the list of :authors in book"
  [book new-author]
  (let [old-authors (:authors book)] 
    (assoc book :authors (conj old-authors new-author))))

(defn 
  alive? 
  "is the author of parameter book alive?"
  [author]
  (not (contains? author :death-year)))

(defn
  element-lengths 
  "return the length of every item in parameter collection"
  [collection]
  (map count collection))

(defn 
  second-elements 
  "return all the second elements of parameter collection"
  [collection]
  (let [helper (fn [v] (get v 1))]
    (map helper collection)))

(defn 
  titles
  "return titles of parameter books"
  [books]
  (let [gettitle (fn [book] (:title book))]
    (map gettitle books)))

(defn 
  monotonic? 
  "is parameter sequence monotonic?"
  [a-seq]
  (if (> (second a-seq) (first a-seq))
    (apply <= a-seq)
    (apply >= a-seq)))

(defn 
  stars
  "return a string of n stars"
  [n]
  (apply str (repeat n \*)))

(defn 
  toggle 
  "remove or add elem to a-set"
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn 
  contains-duplicates? 
  "does parameter sequence contain duplicates?"
  [a-seq]
  (not (= 
          (count (set a-seq)) 
          (count a-seq))))

(defn 
  old-book->new-book 
  "change from old book data representation to new representation, ie authors in a set instead of vector"
  [book]
  (assoc book :authors (set (:authors book))))

(defn 
  has-author? 
  "does book have parameter author?"
  [book author]
  (contains? (:authors book) author))

(defn 
  authors 
  "return all the authors for parameter books"
  [books]
  (apply clojure.set/union (map :authors books)))

(defn 
  all-author-names 
  "return the names of all authors for parameter books"
  [books]
  (let [getauthor (fn [book] map (:name book))]
    (set (map getauthor (authors books)))))

(defn 
  author->string 
  "Return the author as a neat string thingie"
  [author]
  (let [get-birth (str " (" (:birth-year author) " - )")
        get-both  (str " (" (:birth-year author) " - " (:death-year author) ")")
        author-name (:name author)]
    (str author-name (cond 
                       (contains? author :death-year) get-both
                       (contains? author :birth-year) get-birth
                       :else ""))))

(defn 
  authors->string 
  "return a string of authors in a pretty format"
  [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn 
  book->string 
  "returns a nice string description of the authors of the book"
  [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn 
  books->string 
  "Summarize the books with a nice string"
  [books]
  (cond 
    (== 0 (count books)) "No books."
    (== 1 (count books)) (str "1 book. " (apply book->string books) ".")
    :else (str (count books) " books. " (apply (fn [book] (book->string book)) books) ".")))

(defn 
  books-by-author 
  "return the books written by parameter author"
  [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)