(ns structured-data)

(defn do-a-thing [x]
  (let [pl (+ x x)]
    (Math/pow pl pl)))

(defn spiff [v]
  (+ (first v) (get v 2)))

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

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point?
  [[[x1 y1] [x2 y2]]
   [x y]]
  (and (>= x2 x x1) (>= y2 y y1)))

(defn contains-rectangle? [outer [p1 p2]]
  (and (contains-point? outer p1)
       (contains-point? outer p2)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (get book :authors) new-author)))

(defn alive? [author]
  (= nil (:death-year author)))

(defn element-lengths [collection]
  (map #(count %) collection))

(defn second-elements [collection]
  (map #(second %) collection))

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
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors
    (apply conj #{} (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))

(defn all-author-names [books]
         (set (map :name (authors books))))

(defn author->string [author]
  (if (:birth-year author)
  (str (:name author)
       " ("
       (:birth-year author)
       " - "
       (:death-year author)
       ")")
    (:name author)))

(defn authors->string [authors]
  (apply str
         (interpose ", "
                    (map author->string authors))))

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))))

(defn books->string [books]
  (let [bookcount (count books)]
    (if (= 0 bookcount)
      "No books."
      (str
       bookcount
       " book"
       (if (> bookcount 1)
         "s")
       ". "
       (apply
        str
        (interpose ", "
                   (map book->string books)))
       "."))))

(defn books-by-author [author books]
  (filter #(contains? (:authors %) author)
          books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %))
          authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty?
       (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author?
          books))

; %________%
