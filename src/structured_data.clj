(ns structured-data)

(defn
  do-a-thing
  "Does something"
  [x]
  (let [express (+ x x)]
    (Math/pow express express)))

(defn
  spiff
  "Spiff vectors"
  [v]
    (if (< (count v) 3)
     ("?")
     (+ (get v 0) (get v 2))))

(defn
  cutify
  "Vector + <3"
  [v]
    (conj v "<3"))

(defn
  spiff-destructuring
  [v]
     (if (< (count v) 3)
       ("?")
       (let [[a b c] v]
         (+ a c))))

(defn
  point
  [x y]
  [x y])

(defn
  rectangle
  [bottom-left top-right]
  [bottom-left top-right])

(defn
  width
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn
  height
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn
  square?
  [rect]
  (let [hth (height rect)
        wth (width rect)]
          (= hth wth)))

(defn
  area
  [rect]
  (let [hth (height rect)
        wth (width rect)]
          (* hth wth)))

(defn
  contains-point?
  [rectangle point]
  (let [[[bottom-left-x bottom-left-y] [top-right-x top-right-y]] rectangle
        [point-x point-y] point]
         (and (<= bottom-left-x point-x top-right-x) (<= bottom-left-y point-y top-right-y))))

(defn
  contains-rectangle?
  [outer inner]
    (let [[[inner-bottom-left-x inner-bottom-left-y] [tlix inner-top-left-y]] inner
          [[outer-bottom-left-x outer-bottom-left-y] [outer-top-right-x outer-top-right-y]] outer]
            (and (>= inner-bottom-left-x outer-bottom-left-x)
                 (>= inner-bottom-left-y outer-bottom-left-y)
                 (<= tlix outer-top-right-x)
                 (<= inner-top-left-y outer-top-right-y))))


(defn
  title-length
  [book]
 (count (:title book)))

(defn
  author-count
  [book]
  (count (:authors book)))

(defn
  multiple-authors?
  [book]
  (> (author-count book) 1))

(defn
  add-author
  [book new-author]
  (let [ababel (:authors book)]
         (assoc book :authors (conj ababel new-author))))

(defn
  alive?
  [author]
  (not (contains? author :death-year)))

(defn
  element-lengths
  [collection]
  (map count collection))

(defn
  second-elements
  [collection]
    (let [get-second-elements (fn [v] (get v 1))]
      (map get-second-elements collection)))

(defn
  titles
  [books]
    (map :title books))

(defn
  monotonic?
  [a]
  (or
    (apply <= a)
    (apply >= a)))

(defn
  stars
  [n]
  (let [add-n-stars (seq (repeat n "*"))]
    (apply str add-n-stars)))

(defn
  toggle
  [a e]
  (if (contains? a e)
    (disj a e)
    (conj a e)))

(defn
  contains-duplicates?
  [a-seq]
  (let [count-sequence (count a-seq)
        sequence-as-set (set a-seq)
        count-set (count sequence-as-set)
        ]
          (not (= count-sequence count-set))))

(defn
  old-book->new-book
  [book]
  (let [set-of-book (set (:authors book))]
         (assoc book :authors set-of-book)))

(defn
  has-author?
  [book author]
  (let [authors-of-book (:authors book)]
         (contains? authors-of-book author)))


(defn
  authors
  [books]
   (apply clojure.set/union (map :authors books)))

(defn
  all-author-names
  [books]
   (set (map :name (authors books))))

(defn
  author->string
  [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
          (if (contains? author :birth-year)
            (str author-name " (" birth-year " - " death-year ")")
            (str author-name))))

(defn
  authors->string
  [authors]
   (let [authors-as-strings (map author->string authors)
          format-author-strings(apply str (interpose ", " authors-as-strings))]
    format-author-strings))

(defn
  book->string
  [book]
  (let [authors-as-strings (authors->string (:authors book))
        book-title (:title book)
        format-title-and-authors (str book-title  ", written by " authors-as-strings)]
  format-title-and-authors ))

(defn
  books->string
  [books]
  (let [book-count (count books)
        books-as-strings (map  book->string books)
        format-book-list (apply str (interpose ". " books-as-strings))]
            (cond
              (= book-count 0) (str "No books." format-book-list)
              (= book-count 1) (str "1 book. " format-book-list ".")
              (> book-count 1) (str book-count " books. " format-book-list "."))))

(defn
  books-by-author
  [author books]
  (filter  (fn [book] (contains? (:authors book) author)) books))

(defn
  author-by-name
  [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn
  living-authors
  [authors]
  (let [list-of-living-authors (filter (fn [x] (alive? x)) authors)]
    (if (empty? list-of-living-authors)
      ()
      (seq list-of-living-authors))))

(defn
  has-a-living-author?
  [book]
  (let [authors-of-book (authors [book])
        living-authors-of-book (living-authors authors-of-book)]
           (not (empty? living-authors-of-book))))

(defn
  books-by-living-authors
  [books]
    (filter (fn [x] (has-a-living-author? x)) books))
