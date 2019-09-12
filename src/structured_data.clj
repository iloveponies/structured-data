(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj (vec v) "<3"))

(defn spiff-destructuring [[v1 _ v3]]
  (try (+ v1 v3) (catch Exception e)))

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
  (= (rem (apply + (flatten rectangle)) 2) 0))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[r1 r2] [r3 r4]] rectangle
        [p1 p2] point]
    (and (<= r1 p1 r3)
         (<= r2 p2 r4))))

(defn contains-rectangle? [outer inner]
  (let [[[o1 o2] [o3 o4]] outer
        [[i1 i2] [i3 i4]] inner]
    (<= o1 o2 i1 i2 i3 i4 o3 o4)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn all-author-names [books]
  (let [author-names
        (fn [book]
          (map :name (:authors book)))]
    (apply clojure.set/union (map author-names books))))

(defn second-elements [collection]
  (map #(get % 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq)
     (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (into #{} (map :name (authors books))))

(defn author->string [author]
  (if-let [birth (:birth-year author)]
    (str (:name author) " (" birth " - " (:death-year author) ")")
    (str (:name author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
 (let [cnt (count books)]
 (if (= cnt 0) "No books."
 (str cnt (if (> cnt 1) " books. " " book. ")
            (apply str (interpose ", " (map book->string books))) "."))))

(defn books-by-author [author books]
 (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (or (some alive? (:authors book)) false))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
