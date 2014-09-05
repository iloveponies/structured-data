(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

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
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner
        a (point x1 y1)
        b (point x2 y2)]
    (and (contains-point? outer a) (contains-point? outer b))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [a (:authors book)]
    (assoc book :authors (conj a new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [my-func (fn [x] (get x 1))]
    (map my-func collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (let [x (repeat n \*)]
    (apply str x)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [n (count a-seq)
        distinct-n (count (set a-seq))]
    (not= n distinct-n)))

(defn old-book->new-book [book]
  (let [a (:authors book)]
    (assoc book :authors (set a))))

(defn has-author? [book author]
  (let [a (:authors book)]
    (contains? a author)))

(defn authors [books]
  (let [a (fn [book] (:authors book))]
  (apply clojure.set/union (map a books))))

(defn all-author-names [books]
  (let [a (authors books)]
    (set (map :name a))))

(defn author->string [author]
  (let [name (:name author)
        byear (:birth-year author)
        dyear (:death-year author)
        years (if (nil? byear)
                nil
                (str " (" byear " - " dyear ")"))]
    (str name years)))

(defn authors->string [authors]
  (let [a (map author->string authors)]
    (apply str (interpose ", " a))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (apply str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [n (case (count books)
            0 "No books"
            1 "1 book. "
            (str (count books) " books. "))
        b (interpose ". " (map book->string books))]
    (str (apply str n b) ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (let [a (filter #(= (:name %) name) authors)]
    (first a)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (let [a (living-authors (:authors book))]
    (not (empty? a))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
