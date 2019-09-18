(ns structured-data)

(defn
  do-a-thing
  [x]
  (let
    [daburux (+ x x)]
    (Math/pow daburux daburux)))

(defn
  spiff
  [v]
  (+ (get v 0) (get v 2)))

(defn
  cutify
  [v]
  (conj v "<3"))

(defn
  spiff-destructuring
  [[x y z]]
  (+ x z))

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
  (let [[[bottom-leftx bottom-lefty] [top-rightx top-lefty]] rectangle]
    (- top-rightx bottom-leftx)))

(defn
  height
  [rectangle]
  (let [[[bottom-leftx bottom-lefty] [top-rightx top-lefty]] rectangle]
    (- top-lefty bottom-lefty)))

(defn
  square?
  [rectangle]
  (== (width rectangle) (height rectangle)))

(defn
  area
  [rectangle]
  (* (width rectangle) (height rectangle)))

(defn
  contains-point?
  [rectangle point]
  (let [[[bottom-leftx bottom-lefty] [top-rightx top-lefty]] rectangle
        [pointx pointy] point]
    (and (<= bottom-leftx pointx top-rightx) (<= bottom-lefty pointy top-lefty))))

(defn
  contains-rectangle?
  [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1)
         (contains-point? outer point2))))

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
  (let [oldauthors (:authors book)]
    (assoc book :authors (conj oldauthors new-author))))

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
  (let [getter (fn [vect] (get vect 1))]
    (map getter collection)))

(defn
  titles
  [books]
  (map :title books))

(defn
  monotonic?
  [a-seq]
  (let [applier (fn [x] (apply x a-seq))]
    (or (applier <=) (applier >=))))

(defn
  stars
  [n]
  (apply str (repeat n "*")))

(defn
  toggle
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn
  contains-duplicates?
  [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn
  old-book->new-book
  [book]
  (let [old-authors (:authors book)]
    (assoc book :authors (set old-authors))))

(defn
  has-author?
  [book author]
  (contains? (:authors book) author))

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
  (let [authname (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
     death (str authname " (" birth " - " death")")
     birth (str authname " (" birth " - )")
     :else (str authname))))

(defn
  authors->string
  [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn
  book->string
  [book]
  (let [bname (:title book)
        bautors (:authors book)]
    (apply str [bname ", written by " (authors->string bautors)])))

(defn
  books->string
  [books]
  (let [booksep (fn [bks] (apply str(interpose ". " (map book->string bks))))
        bookorbooks (fn [string] (apply str [(count books) " " string ". " (booksep books) "."]))]
     (cond
      (== 0 (count books)) "No books."
      (== 1 (count books)) (bookorbooks "book")
      :else (bookorbooks "books"))))

(defn
  books-by-author
  [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn
  author-by-name
  [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn
  living-authors
  [authors]
  (filter alive? authors))

(defn
  has-a-living-author?
  [book]
  (not (empty? (living-authors (:authors book)))))

(defn
  books-by-living-authors
  [books]
  (filter has-a-living-author? books))

; %________%
