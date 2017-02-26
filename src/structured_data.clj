(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z)))

(defn spiff [v]
  (spiff-destructuring v))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn square? [rectangle]
    (if (= (width rectangle) (height rectangle))
      true
      false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let
      [[[x1 y1] [x2 y2]] rectangle
      [x3 y3] point]
    (if (and(<= x1 x3 x2) (<= y1 y3 y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer (point x1 y1))
             (contains-point? outer (point x2 y2)))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (:death-year author) false true))

(defn counterseq [x]
  (count x))

(defn element-lengths [collection]
  (map counterseq collection))

(defn second-elements [collection]
  (let [counter (fn [x] (get x 1))]
    (map counter collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
    (let [author
         (fn [book] (:authors book))]
    (set (apply clojure.set/union (map author books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year)
  (if (contains? author :death-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (str (:name author) " (" (:birth-year author) " - )"))
  (str (:name author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

 (defn books->string [books]
  (cond
    (= 1 (count books)) (str "1 book. " (apply book->string books) ".")
    (< 1 (count books)) (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    :else (str "No books.")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
