(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (if(> (count v) 2)
    (+ (get v 0) (get v 2))
    nil))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let[[x y z] v]
  (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[x y] rectangle]
    (- (get y 0) (get x 0))))

(defn height [rectangle]
  (let [[x y] rectangle]
    (- (get y 1) (get x 1))))

(defn square? [rectangle]
  (if(= (height rectangle)(width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[x y] rectangle]
    (if (and (<= (get x 0) (get point 0) (get y 0)) (<= (get x 1) (get point 1) (get y 1)))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let[[x y] inner]
    (if (contains-point? outer x)
      (if (contains-point? outer y)
        true
        false)
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (<= 2 (count (:authors book)))
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj(:authors book) new-author)))

(defn alive? [author]
  (if (= nil (:death-year author))
    true
    false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn[x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))

(defn stars [n]
   (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [s (set a-seq)]
    (if(== (count s)(count a-seq))
      false
      true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
    (if(contains? author :birth-year)
      (str (:name author)" (" (:birth-year author) " - " (:death-year author) ")")
      (str (:name author))))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [s (apply str (interpose ", "(map book->string books)))]
    (if( == 0 (count books))
      (str "No books.")
      (if (== 1 (count books))
        (str "1 book. " s ".")
        (str (count books) " books. " s ".")))))

(defn books-by-author [author books]
  (let[by (fn[x] (has-author? x author))]
    (filter by books)))

(defn author-by-name [name authors]
  (let[check (fn[x] (= name (:name x)))]
    (first (filter check authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if(= nil (first(living-authors (:authors book))))
    false
    true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))