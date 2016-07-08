(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

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
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
         a (height rectangle)
         b (width rectangle)]
    (* a b)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [a b] point]
    (and (<= x1 a x2)
         (<= y1 b y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and (<= x1 x3 x4 x2)
         (<= y1 y3 y4 y2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let [newauthors (conj (:authors book) new-author)]
    (assoc book :authors newauthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [lasku (fn [x] (get x 1))]
  (map lasku collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [lkm (count a-seq)]
    (> lkm (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (set (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [x (:name author)
        a (:birth-year author)
        b (:death-year author)]
      (cond
        (not (contains? author :birth-year))(str x)
        (not (contains? author :death-year))(str x " ("a " - )")
        :else (str x " ("a " - " b ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [x (authors->string (:authors book))
        b (:title book)]
    (str b ", written by " x)))

(defn books->string [books]
  (let [lkm (count books)]
    (cond
      (== lkm 0) "No books."
      (== lkm  1) (str "1 book. " (apply book->string books) ".")
      :else (str lkm " books. "
                 (apply str
                        (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))
