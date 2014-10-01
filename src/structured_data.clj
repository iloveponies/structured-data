(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (let [[[a b] [c d]] rectangle]
    (- c a)))

(defn height [rectangle]
  (let [[[a b] [c d]] rectangle]
    (- d b)))

(defn square? [rectangle]
  (if   (== (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[a b] [c d]] rectangle
        [e f] point]
    (if (and (<= a e c) (<= b f d))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[a b] [c d]] inner]
    (if (and (contains-point? outer (point a b)) (contains-point? outer (point c d)))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< (author-count book) 2)
    false
    true
    ))

(defn add-author [book new-author]
  (let [author (:authors book)]
    (assoc book :authors (conj author new-author))))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secondelement (fn [x] (get x 1))]
    (map secondelement collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (let [author (set (:authors book))]
    (assoc book :authors author)))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (if (contains? authors author)
      true
      false)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (let [author (authors books)]
    (set (map :name author))))

(defn author->string [author]
  (let [name (:name author)
        life (str " (" (:birth-year author) " - " (:death-year author) ")") ]
    (if (:birth-year author)
      (str name life)
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string(get book :authors))))

(defn books->string [books]
  (cond (= (count books) 0) (str "No books.")
        (= (count books) 1) (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
        :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
