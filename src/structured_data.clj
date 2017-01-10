(ns structured-data)

(defn do-a-thing [x]
  (let [tx (+ x x)]
    (Math/pow tx tx)))

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
  (let [[bl tr] rectangle
        [blx bly] bl
        [trx trz] tr]
    (- trx blx)))

(defn height [rectangle]
  (let [[bl tr] rectangle
        [blx bly] bl
        [trx trz] tr]
    (- trz bly)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[bl tr] rectangle
        [blx bly] bl
        [trx tryy] tr
        [px, py] point
        incx (<= blx px trx)
        incy (<= bly py tryy)]
    (and incx incy)))

(defn contains-rectangle? [outer inner]
  (let [[ilb irt] inner]
   (and (contains-point? outer ilb) (contains-point? outer irt))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        nauthors (conj authors new-author)]
  (assoc book :authors nauthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

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
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (if (contains? authors author)
      true
      false)))

(defn authors [books]
   (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [author-names
         (map :name (authors books))]
    (set author-names)))

(defn author->string [author]
  (let [n (:name author)
        by (:birth-year author)
        dy (:death-year author)]
  (if (contains? author :death-year)
    (str n " (" by " - " dy ")")
    (if (contains? author :birth-year)
      (str n " (" by " - )")
      n))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [cnt (count books)
        term (if (== 0 cnt)
               "No books"
               (if (== 1 cnt)
                 "1 book. "
                 (str cnt " books. ")))]
    (str term (apply str (interpose ", " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
