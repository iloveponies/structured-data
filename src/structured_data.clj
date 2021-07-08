(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (+
   (get v 0)
   (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (=
   (width rectangle)
   (height rectangle)))

(defn area [rectangle]
  (*
   (width rectangle)
   (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and
     (<= x1 x x2)
     (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and
     (contains-point? outer bottom-left)
     (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (>
   (author-count book)
   1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc
     book
     :authors
     new-authors)))

(defn alive? [author]
  (not
   (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [snd (fn [x] (get x 1))]
    (map snd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (=
          (count a-seq)
          (count a-set)))))

(defn old-book->new-book [book]
  (let [author-v (:authors book)
        author-set (set author-v)]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains?
   (:authors
    (old-book->new-book book))
   author))

(defn authors [books]
  (apply
   clojure.set/union
   (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (get author :birth-year "")
        death-year (get author :death-year "")
        year (if (= birth-year "") "" (str " (" birth-year " - " death-year ")"))]
    (str name year)))

(defn authors->string [authors]
  (apply str
         (interpose
          ", "
          (map author->string authors))))

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))))

(defn books->string [books]
  (let [number-of-books (count books)]
    (cond
      (= number-of-books 0) "No books."
      (= number-of-books 1) (str
                             "1 book. "
                             (book->string (first books))
                             ".")
      :else (str
             number-of-books
             " books. "
             (apply str (interpose ". " (map book->string books)))
             "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter
           (fn [author] (= name (:name author)))
           authors)))

(defn living-authors [authors]
  (filter
    (fn [author] (alive? author))
    authors))

(defn has-a-living-author? [book]
  (not
    (empty?
      (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
