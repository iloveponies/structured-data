(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
     (Math/pow a a)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (boolean (== (height rectangle) (width rectangle))))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
    (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (let [func (fn [x] (count x))]
    (map func collection)))

(defn second-elements [collection]
  (let [clear (fn [x] (get x 1))]
    (map clear collection)
    ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (concat (repeat n "*"))))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        year (cond
              (:death-year author) (str " (" (:birth-year author) " - " (:death-year author) ")")
              (:birth-year author) (str " (" (:birth-year author) " - )")
              :else "")]
        (str name year)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [name (str (:title book) ", written by ")
        writers (authors->string (:authors book))]
        (str name writers)))

(defn books->string [books]
  (let [amount (count books)
        text (if (< amount 1) "No books." (str amount " book" (if (> amount 1) "s. " ". ")))
        info (if (< amount 1) "" (apply str (interpose ", " (map book->string books))))]
    (str text info (if (> amount 0) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [findName? (fn [x] (= (:name x) name))
    list (filter (fn [y] (findName? y)) authors)]
    (if (= (count list) 0 ) nil (first list))))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (let [list (living-authors (:authors book))]
    (not (empty? list))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
