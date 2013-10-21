(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)))

(defn spiff [v]
;;  (let [x (get v 0)
;;       y (get v 3)]
;;    (+ x y )))
  (cond
    (and (vector? v) (> (count v) 2)) (+ (get v 0) (get v 2))
    :else nil))



(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[z1 z2] point]
      (and (<= x1 z1 x2) (<= y1 z2 y2)))))


(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[z1 b1] [z2 b2]] inner]
      (and (contains-point? outer [z1 b1]) (contains-point? outer [z2 b2])))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1 ))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))


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
  (let [x (count a-seq)]
    (let [y (count (set a-seq))]
      (> x y))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))


(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (str (:name author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [numbooks (count books)
        text
         (cond
          (== 0 numbooks) "No books"
          (== 1 numbooks) "1 book. "
          :else (str numbooks " books. "))]
    (let [booksstring (apply str (interpose ", " (map book->string books)))]
      (str text booksstring "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first
   (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [living (count (living-authors (:authors book)))]
    (> living 0)))


(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; @________@



