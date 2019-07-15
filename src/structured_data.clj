(ns structured-data)

(defn do-a-thing [x]
  (let [square (+ x x)]
    (Math/pow square square)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (let [[[x1] [x2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2)
         (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
      (and (contains-point? outer point1)
           (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n](monotonic? [1 2 1 0])
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [count-seq (count a-seq)
        count-set (count (set a-seq))]
    (> count-seq count-set)))

(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors-sets (map :authors books)]
    (apply clojure.set/union authors-sets)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str \( (:birth-year author) " - " (:death-year author) \))]
    (if (contains? author :birth-year)
      (str name " " years)
      name)))

(defn authors->string [authors]
  (let [formatted-authors (map author->string authors)
        separated-authors (interpose ", " formatted-authors)]
    (apply str separated-authors)))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [quantity (count books)]
    (if (== 0 quantity)
      "No books."
      (let [formatted-books (map book->string books)
            representation (apply str (interpose ". " formatted-books))]
        (if (== 1 quantity)
          (str "1 book. " representation \.)
          (str quantity " books. " representation \.))))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [is-author? (fn [author] (= name (:name author)))]
    (first (filter is-author? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
