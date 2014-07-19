(ns structured-data)


;; http://iloveponies.github.io/120-hour-epic-sax-marathon/structured-data.html

(defn do-a-thing [x]
  (let [double-val (+ x x)]
    (Math/pow double-val double-val)))

(defn spiff [v]
  (let [p1 (get v 0 0)
        p2 (get v 2 0)]
    (+ p1 p2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[p1 _ p2 & _]]
  (+ (if p1 p1 0) (if p2 p2 0)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (Math/abs (- x2 x1)))

(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs (- y2 y1)))

(defn square? [rect]
  (= (width rect) (height rect)))

(defn area [rect]
   (* (width rect) (height rect)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [px py] point]
    (and (>= x2 px x1)
         (>= y2 py y1))))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and (contains-point? outer bl)
         (contains-point? outer tr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-length [element]
  (count element))

(defn element-lengths [collection]
  (map element-length collection))

(defn second-element [collection]
  (second collection))

(defn second-elements [collection]
  (map second-element collection))

;; Exercise 17
;; Vector of Map -> LazySeq of String
;; Returns the title of every book in the vector, () if no books
(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))


;; Exercise 23
;; Book Author -> Boolean
;; Returns true if Author took part in writing Book
(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

;; Exercise 24
;; Vector of Book -> Set of Author
(defn authors [books]
  (let [author-names
        (fn [book] (:authors (old-book->new-book book)))]
    (apply clojure.set/union (map author-names books))))

;; Exercise 25
;; Vector of Book -> Set of String
;; Returns a set containing the name, in a string, of each author in the input vector
(defn all-author-names [books]
  (set (map :name (authors books))))

;; Exercise 26
;; Set -> String
;; Outputs a string containing the name of the author + birth and death if available
(defn author->string [{:keys [name birth-year death-year]}]
  (let [birth-text (if birth-year
                     (str " (" birth-year " ")
                     "")
        death-text (if death-year
                     (str "- " death-year ")")
                     (if birth-year
                       "- )"
                       ""))]
    (str name birth-text death-text)))


;; Exercise 27
;; List of Maps -> String
;; Outputs a string holding all author information separated by commas
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


;; Exercise 28
;; Book -> String
;; Returns a string in the format "<>, written by <author(s)"
(defn book->string [{:keys [title authors]}]
  (str title ", written by " (authors->string authors)))

;; Exercise 29
;; List of Book -> String
;; Returns a string holding info on every book in the list or "No books"
(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [num-books (count books)
          book-spelling (if (= num-books 1)
                          "book"
                          "books")
          book-text (apply str (interpose ". " (map book->string books)))]
      (str num-books
           " "
           book-spelling
           ". "
           book-text
           "."))))

;; Exercise 30
;; Author, Vector of Book -> Sequence of Book
;; Returns a sequence of all books in the input vector written by Author
(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


;; Exercise 31
;; String, Set of Author -> Author or nil
;; Returns the author identified by name if found in the input set, nil otherwise
(defn author-by-name [name authors]
  (let [find-author (fn [author]
                      (= name (:name author)))
        results (filter find-author authors)]
    (if (empty? results)
      nil
      (first results))))

;; Exercise 32
;; Set of Author -> Sequence of Author
;; Returns a sequence of all authors in the input set that are still alive
(defn living-authors [authors]
  (filter alive? authors))

;; Exercise 33
;; Book -> Boolean
;; Returns true if any authors of Book is still alive, false otherwise
(defn has-a-living-author? [book]
  (not (empty? (living-authors (authors [book])))))

;; Exercise 34
;; Vector of Book -> Sequence of Book
;; returns a sequence holding all books found in the input vector and has living authors
(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
