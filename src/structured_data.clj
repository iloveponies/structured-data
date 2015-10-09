(ns structured-data)

(defn do-a-thing [x]
  (let [doble-x (+ x x)]
    (Math/pow doble-x doble-x)))

(defn spiff [v]
  (let [first-element (get v 0)
        third-element (get v 2)]
    (+ first-element third-element)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first-element y third-element] v]
    (+ first-element third-element)))

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
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
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
  (not= (count a-seq)
     (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors-book
        (fn [book] (:authors book))]
    (apply clojure.set/union (map authors-book books))))

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name       (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        years      (if birth-year
                     (str " (" birth-year " - " death-year ")")
                     "")]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title   (:title book)
        authors (authors->string (:authors book))]
    (str
      title
      ", written by "
      authors)))

(defn books->string [books]
  (let [detail-books (apply str (interpose ". " (map book->string books)))
        number-books (count books)
        string-number-books (apply str number-books " book"
                                     (when (> number-books 1) "s") ". ")]
    (if (empty? books)
      "No books."
      (str string-number-books detail-books "."))))

(defn books-by-author [author books]
  (let [has-author-looked-for?
        (fn [book] (has-author? book author))]
    (filter has-author-looked-for? books)))

(defn author-by-name [name authors]
  (let [look-for-name
          (fn [author] (= name (:name author)))
        authors-found
          (filter look-for-name authors)]
    (if (empty? authors-found)
      nil
      (first authors-found))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
