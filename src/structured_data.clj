(ns structured-data)

(defn do-a-thing [x]
  (let [sumx (+ x x)]
    (Math/pow sumx sumx)))

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

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [[[x1 y1] [x2 y2]]]
  (if (= (- x2 x1) (- y2 y1))
    true
    false))

(defn area [[[x1 y1] [x2 y2]]]
  (* (- y2 y1) (- x2 x1)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (if (and (<= x1 px x2) (<= y1 py y2))
    true
    false))

(defn contains-rectangle? [outer [bl tr]]
  (if (and (contains-point? outer bl) (contains-point? outer tr))
    true
    false))

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
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq))
    true
    false))

(defn old-book->new-book [book]
  (let [newauth (fn [x] (set x))]
    (assoc book :authors (newauth (:authors book)))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [a-by (:birth-year author)
        a-dy (:death-year author)
        a-name (:name author)
        a-years (apply str (interpose " - " [a-by a-dy]))]
    (if (contains? author :birth-year)
      (str a-name " (" a-years ")")
      (str a-name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [nbooks (count books)]
    (if (empty? books)
      (str "No books.")
      (cond
        (> nbooks 1) (str nbooks " books. " (apply str (map book->string books)) "." )
        (= nbooks 1) (str nbooks " book. " (apply str (map book->string books)) ".")))))

(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books))

(defn author-by-name [aname authors]
  (first (filterv (fn [x] (= (:name x) aname)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (if (empty? (filter (fn [x] (alive? x)) (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
