(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v0 v1 v2] v]
    (+ v0 v2)))

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
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xx yy] point]
    (if (and (<= x1 xx x2) (<= y1 yy y2))
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
  (if (< 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [snd (fn [x] (first (rest x)))]
    (map snd collection)))

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
  (let [conjunto (set a-seq)]
    (if (= (count a-seq) (count conjunto))
      false
      true)))

(defn old-book->new-book [book]
  (let [vec-authors (:authors book)
        set-authors (set vec-authors)]
    (assoc book :authors set-authors)))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
     death-year (str name " (" birth-year " - " death-year ")")
     birth-year (str name " (" birth-year " - )")
     :else (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [num-books (count books)]
    (cond
     (= 0 num-books) "No books."
     (= 1 num-books) (str "1 book. " (book->string (get books 0)) ".")
     :else (str num-books " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (let [auth (filter #(= name (:name %)) authors)]
    (if (empty? auth)
      nil
      (first auth))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
