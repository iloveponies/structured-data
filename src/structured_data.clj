(ns structured-data)

(defn do-a-thing [x]
  (let [summedX (+ x x)]
    (Math/pow summedX summedX)))

(defn spiff [v]
  (let [firstN (get v 0)
        thirdN (get v 2)]
    (+ firstN thirdN)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (let [width-r (width rectangle)
    height-r (height rectangle)]
    (if (== width-r height-r)
      true
      false)))

(defn area [rectangle]
  (let [width-r (width rectangle)
        height-r (height rectangle)]
    (* width-r height-r)))

(defn contains-point? [rectangle point]
  (let [[x y] point [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 x x2)
         (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[innerx1 innery1] [innerx2 innery2]] inner]
    (and (contains-point? outer [innerx1 innery1])
         (contains-point? outer [innerx2 innery2]))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-elem (fn [x] (get x 1))]
    (map second-elem collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
 (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-names
        (fn [book] (set (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (str (:name author))))

(defn authors->string [authors]
  (let [author
        (fn [person] (map author->string person))]
    (apply str (interpose ", " (author authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-info
        (fn [book] (map book->string book))]
    (cond
      (> (count books) 1)      (apply str (apply str (count books) " books. " (interpose ". " (book-info books))) ".")
      (== (count books) 1)     (apply str (apply str (count books) " book. " (interpose ". " (book-info books))) ".")
      :else                    (apply str "No books.")
      )))

(defn books-by-author [author books]
  (let [is-author
        (fn [book] (has-author? book author))]
    (filter is-author books)))

(defn author-by-name [name authors]
   (let [authors-list
        (fn [author] (= name (str (:name author))))]
    (if (= (filter authors-list authors) ())
      nil
      (first (filter authors-list authors)))))

(defn living-authors [authors]
  (let [is-alive?
        (fn [author] (alive? author))]
    (filter is-alive? authors)))


(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (let [writer-alive
        (fn [book] (has-a-living-author? book))]
    (filter writer-alive books)))

; %________%
