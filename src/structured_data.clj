(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

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

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (if (and (<= x1 xp x2) (<= y1 yp y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if (and (contains-point? outer p1) (contains-point? outer p2))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sequencer (fn [x] (get x 1))]
    (map sequencer collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if (== (count a-set) (count a-seq))
      false
      true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (let [authors (fn [book] (:authors book))]
    (apply clojure.set/union (map authors books))))

(defn all-author-names [books]
  (let [name (fn [author] (:name author))]
    (set (map name (authors books)))))

(defn author->string [author]
  (let [name (:name author)
        years (str (:birth-year author) " - " (:death-year author))]
    (if (nil? (:birth-year author))
      (str name)
      (str name " (" years ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (== (count books) 0)
    (str "No books.")
    (str (count books) " book"
         (if (> (count books) 1) "s") ". "
         (apply str (interpose ". " (map book->string books)))
         ".")))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (let [filtered-authors (filter (fn [author] (= (:name author) name)) authors)]
    (if (> (count filtered-authors) 0)
      (first filtered-authors)
      nil)))

(defn living-authors [authors]
  (filter (fn [author] (= (:death-year author) nil)) authors))

(defn has-a-living-author? [book]
  (let [living (living-authors (:authors book))]
    (not (empty? living))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
