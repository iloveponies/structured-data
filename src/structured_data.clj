(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (if (= (- (height rectangle) (width rectangle)) 0)
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[xp yp] point]
      (if (and (<= x1 xp x2) (<= y1 yp y2))
        true
        false))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (if (and (contains-point? outer point1) (contains-point? outer point2))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1)
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
  (let [second (fn [x] (first (rest x)))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (apply >= a-seq)
    true
    (if (apply <= a-seq)
      true
      false)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (into #{} a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (into #{} (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (let [into-set (fn [book] (into #{} (:authors book)))]
    (apply clojure.set/union (map into-set books))))

(defn all-author-names [books]
  (let [get-name (fn [author] (:name author))]
    (set (map get-name (authors books)))))

(defn author->string [author]
  (if (contains? author :death-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (if (contains? author :birth-year)
      (str (:name author) " (" (:birth-year author) " - )")
      (str (:name author)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (case (count books)
    0 (str "No books.")
    1 (str "1 book. " (book->string (first books)) ".")
    (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (if (= (count (filter (fn [author] (= name (:name author))) authors)) 0)
    nil
    (first (filter (fn [author] (= name (:name author))) authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (filter alive? (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
