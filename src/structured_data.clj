(ns structured-data)

(defn do-a-thing [x]
  (let [x-double (+ x x)]
    (Math/pow x-double x-double)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle)
     (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2)
       (<= y1 y y2)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner
        w                 (width inner)
        h                 (height inner)]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [(+ x1 w) y1])
         (contains-point? outer [x1 (+ y1 h)])
         (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors     (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [[a b]] b)]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq)
     (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (let [author-sets (map :authors books)]
    (apply clojure.set/union author-sets)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name  (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (str name
         (if birth
           (str " (" birth " - " death ")")
           ""))))

(defn authors->string [authors]
  (let [authors-str (map author->string authors)]
    (apply str (interpose ", " authors-str))))

(defn book->string [book]
  (let [title       (:title book)
        authors     (:authors book)
        authors-str (authors->string authors)]
    (str title ", written by " authors-str)))

(defn books->string [books]
  (let [num-books        (count books)
        books-num-string (cond
                           (= 0 num-books) "No books."
                           (= 1 num-books) "1 book."
                           :else           (str num-books " books."))
        book-strs        (map book->string books)
        book-period-strs (map (fn [book-str] (str book-str ".")) book-strs)
        book-joined-strs (apply str (interpose " " book-period-strs))]
    (str books-num-string
         (if (> num-books 0)
           (str " " book-joined-strs)
           ""))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author)
                                 name))
                 authors)))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
