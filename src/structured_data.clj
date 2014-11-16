(ns structured-data)

(defn do-a-thing [x]
  (let[xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
      (if (and a b)
        (+ a b)
        \?)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (if (and a c)
    (+ a c)
    (\?)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [p1 p2]]
  (and (<= x1 p1 x2) (<= y1 p2 y2)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2]  inner]
  (and (contains-point? outer p1)
       (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
  (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [munge (fn [x] (count x))]
  (map munge collection)))

(defn second-elements [collection]
  (let [second-element (fn [vector] (get vector 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (if (:birth-year author)
      (str name " " years)
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        author-names (authors->string (:authors book))]
    (str title ", written by " author-names)))

(defn books->string [books]
  (let [book-list (str (apply str (interpose ". " (map book->string books))) ".")
        count (count books)]
   (cond
     (== count 0) "No books."
     (== count 1) (str count " book. " book-list)
     (> count 1) (str count " books. " book-list))))

(defn books-by-author [author books]
  (let [has-author? (fn [book] 
                      (contains? (:authors book) author))]
    (filter has-author? books)))

(defn author-by-name [name authors]
  (let [named? (fn [author]
                (= name (:name author)))]
    (first (filter named? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
