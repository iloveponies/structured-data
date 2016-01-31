(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [num1 (first v)
        num3 (get v 2)]
    (+ num1 num3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[f s t] v] (+ f t)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[lower upper] inner]
    (and (contains-point? outer lower)
         (contains-point? outer upper))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [scnd (fn [coll] (get coll 1))]
    (map scnd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [mono-inc? (apply <= a-seq)
        mono-dec? (apply >= a-seq)]
    (or mono-inc? mono-dec?)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [size (count a-seq)
        ssize (count (set a-seq))]
    (< ssize size)))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
   (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [all-authors (authors books)]
    (set (map :name all-authors))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
      (and (nil? birth-year) (nil? death-year)) name
      (nil? death-year) (str name " (" birth-year " - )")
      :else (str name " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors-string (authors->string (:authors book))]
    (str title ", written by " authors-string)))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [book-count (count books)
          unit (if (> book-count 1) "books" "book")
          book-strings (map book->string books)]
      (str book-count " " unit ". " (apply str (interpose ". " book-strings)) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (if (seq (living-authors authors))
      true
      false)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
