(ns structured-data)

(defn do-a-thing [x]
  (let [twoX (+ x x)]
    (Math/pow twoX twoX)))

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
  (let [[[xbr ybr] [xtl ytl]] rectangle]
    (- xtl xbr)))

(defn height [rectangle]
  (let [[[xbr ybr] [xtl ytl]] rectangle]
    (- ytl ybr)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[xbr ybr] [xtl ytl]] rectangle
        [xp yp] point]
    (and (<= xbr xp xtl) (<= ybr yp ytl))))

(defn contains-rectangle? [outer inner]
    (let [[br tl] inner]
      (and (contains-point? outer br)
           (contains-point? outer tl))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        modified (conj authors new-author)]
    (assoc book :authors modified)))


(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [xs] (first (rest xs)))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author] ()
  (cond
   (contains? author :death-year) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
   (contains? author :birth-year) (str (:name author) " (" (:birth-year author) " - )")
   (contains? author :name)       (str (:name author))
   :else (str "")))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (book :authors))))

(defn books->string [books]
  (cond
   (empty? books)      "No books."
   (= (count books) 1) (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
   (> (count books) 1) (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
