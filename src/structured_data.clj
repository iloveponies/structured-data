(ns structured-data)

(defn do-a-thing [x]
 (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
 (let [first (get v 0)
       third (get v 2)]
  (+ first third)))

(defn cutify [v]
 (conj v "<3"))

(defn spiff-destructuring [v]
 (let [[first second third] v]
  (+ first third)))

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
 (let [[[x1 y1] [x2 y2]] rectangle
       height (- y2 y1)
       width (- x2 x1)]
  (= height width)))

(defn area [rectangle]
 (let [[[x1 y1] [x2 y2]] rectangle
       height (- y2 y1)
       width (- x2 x1)]
  (* height width)))

(defn contains-point? [rectangle point]
 (let [[[x1 y1] [x2 y2]] rectangle
       [pointX pointY] point]
  (and (<= x1 pointX x2) (<= y1 pointY y2))))

(defn contains-rectangle? [outer inner]
 (let [[innerPoint1 innerPoint2] inner]
  (and (contains-point? outer innerPoint1)
       (contains-point? outer innerPoint2))))

(defn title-length [book]
 (count (:title book)))

(defn author-count [book]
 (count (:authors book)))

(defn multiple-authors? [book]
 (> (author-count book) 1))

(defn add-author [book new-author]
 (let [new-authors (conj (:authors book) new-author)]
  (assoc book :authors new-authors)))

(defn alive? [author]
 (not (contains? author :death-year)))

(defn element-lengths [collection]
 (map count collection))

(defn second-elements [collection]
 (let [secondify (fn [vec] (get vec 1))]
  (map secondify collection)))

(defn titles [books]
 (map :title books))

(defn monotonic? [a-seq]
 (or (apply <= a-seq)
     (apply >= a-seq)))

(defn stars [n]
 (apply str (repeat n \*)))

(defn toggle [a-set elem]
 (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn contains-duplicates? [a-seq]
 (let [sequence-elements (count a-seq)
       set-elements (count (set a-seq))]
  (not (= sequence-elements set-elements))))

(defn old-book->new-book [book]
 (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
 (let [book-authors (:authors book)]
  (contains? book-authors author)))

(defn authors [books]
 (let [authors (map :authors books)]
  (apply clojure.set/union authors)))

(defn all-author-names [books]
 (set (map :name (authors books))))

(defn author->string [author]
 (let [name (:name author)
       birth-year (:birth-year author)
       death-year (:death-year author)]
  (cond
   (and birth-year death-year) (str name " (" birth-year " - " death-year ")")
   birth-year (str name " (" birth-year " - )")
   :else name)))

(defn authors->string [authors]
 (let [author-strings (map author->string authors)]
  (apply str (interpose ", " author-strings))))

(defn book->string [book]
 (let [book-title (:title book)
       authors (authors->string (:authors book))]
  (str book-title ", written by " authors)))

(defn books->string [books]
 (let [number-of-books (count books)]
  (cond
   (= 0 number-of-books) (str "No books.")
   (= 1 number-of-books) (str "1 book. " (book->string (first books)) ".")
   :else (str number-of-books " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
 (let [book-has-author? (fn [book] (has-author? book author))]
  (filter book-has-author? books)))

(defn author-by-name [name authors]
 (let [authors-match (filter (fn [author] (= name (:name author))) authors)]
  (if (empty? authors-match)
   nil
   (first authors-match))))

(defn living-authors [authors]
 (filter alive? authors))

(defn has-a-living-author? [book]
 (let [book-authors (:authors book)
       living-authors (filter alive? book-authors)]
  (not (empty? living-authors))))

(defn books-by-living-authors [books]
 (filter has-a-living-author? books))

; %________%
