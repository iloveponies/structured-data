(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (if (not (or (nil? x) (nil? y)))
      (+ x y)
      0)))

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
  "Resta los componente x1 x de un rectangulo
  para calcular su width"
  (let [[[x _] [x1 _]] rectangle]
    (- x1 x)))

(defn height [rectangle]
  "Resta los componentes y de un rectangulo
  para calcular su height"
  (let [[[_ y] [_ y1]] rectangle]
    (- y1 y)))

(defn square? [rectangle]
  (let [[[x y] [x1 y1]] rectangle
        p1 (- x y)
        p2 (- x1 y1)]
    (= p1 p2)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x1 y2]] inner
        point-a [x1 y2]
        point-b [x1 y2]]
    (and (contains-point? outer point-a) (contains-point? inner point-b))))

(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (let [num-authors (author-count book)]
    (if (> num-authors 1)
      true
      false)))

(defn add-author [book new-author]
  (let [current-authors (:authors book)]
    (assoc book :authors (conj current-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [e] (get e 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [before-set (count a-seq)
        after-set (count (set a-seq))]
    (not (= before-set after-set))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [tname (:name author)
        birth (:birth-year author)
        deadth (:death-year author)]
    (if (not (nil? birth))
      (str tname " (" birth " - " deadth ")")
      (str tname))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [num-books (count books)
        text (if (> num-books 1)
               "books"
               "book")]
    (if (= 0 num-books)
      "No books."
      (str num-books " " text ". " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
