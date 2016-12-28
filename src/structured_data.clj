(ns structured-data)

(defn do-a-thing
  [x]
  (let [thing (+ x x)]
    (Math/pow thing thing)))

(defn spiff
  [v]
  (when (>= (count v) 3)
    (let [first (first v)
          third (get v 2)]
      (+ first third))))

(defn cutify
  [v]
  (conj v "<3"))

(defn spiff-destructuring
  [v]
  (when (>= (count v) 3)
    (let [[x _ y] v]
      (+ x y))))

(defn point
  [x y]
  [x y])

(defn rectangle
  [bottom-left top-right]
  [bottom-left top-right])

(defn width
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square?
  [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))

(defn area
  [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point?
  [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (if (and (>= x2 xp x1)
             (>= y2 yp y1))
      true
      false)))

(defn contains-rectangle?
  [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (if (and (contains-point? outer (point x3 y3))
             (contains-point? outer (point x4 y4)))
      true
      false)))

(defn title-length
  [book]
  (count (:title book)))

(defn author-count
  [book]
  (count (:authors book)))

(defn multiple-authors?
  [book]
  (if (< 1 (author-count book))
    true
    false))

(defn add-author
  [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive?
  [author]
  (not (contains? author :death-year)))

(defn element-lengths
  [collection]
  (map count collection))

(defn second-elements
  [collection]
  (let [sec (fn [v] (second v))]
    (map sec collection)))

(defn titles
  [books]
  (map :title books))

(defn monotonic?
  [a-seq]
  (if (or (apply <= a-seq)
          (apply >= a-seq))
    true
    false))

(defn stars
  [n]
  (apply str (repeat n "*")))

(defn toggle
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates?
  [a-seq]
  (let [num (count a-seq)
        setify (count (set a-seq))]
    (if (> num setify)
      true
      false)))

(defn old-book->new-book
  [book]
  (assoc book :authors (set (:authors book))))

(defn has-author?
  [book author]
  (let [auth (:name author)
        bk (set (map :name (:authors book))) ]
    (contains? bk auth)))

(defn authors
  [books]
  (let [auth (fn [book] (:authors book))]
    (set (apply concat (map auth books)))))

(defn all-author-names
  [books]
  (set (map :name (authors books))))

(defn author->string
  [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
      death (str name " (" birth " - " death ")")
      birth (str name " (" birth " - )")
      :else name)))

(defn authors->string
  [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string
  [book]
  (let [title (:title book)
        authors (map author->string (authors [book]))
        str-authors (apply str (interpose ", " authors))]
    (format "%s, written by %s" title str-authors)))

(defn books->string
  [books]
  (cond
    (= 0 (count books)) "No books."
    (= 1 (count books)) (str "1 book. " (apply str (map book->string books)) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author
  [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name
  [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors
  [authors]
  (filter (fn [auth] (alive? auth)) authors))

(defn has-a-living-author?
  [book]
  (not (empty? (living-authors (authors [book])))))

(defn books-by-living-authors
  [books]
  (filter (fn [book] (has-a-living-author? book)) books))
