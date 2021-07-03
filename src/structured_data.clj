(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x )]
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
  (let [[[x1 y1] [x2 y2]] rectangle]
        (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
      (and (<= x1 p1 x2) (<= y1 p2 y2))))


(defn contains-rectangle? [outer inner]
  (let [[[x3 y3] [x4 y4]] outer
       [[x1 y1] [x2 y2]] inner]
        (and (contains-point? outer [x1 y1])
             (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (>= (author-count book) 2))

(defn add-author [book new-author]
  (let [orig-authors (:authors book)
        new-authors (conj orig-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [a-seq] (get a-seq 1))]
    (map second collection)
  ))

(defn titles [books]
  (let [title (fn [book] (:title book))]
    (map title books))
  )

(defn monotonic? [a-seq]
    (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)]
    (assoc book :authors (set (concat old-authors)))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [author-list (authors books)]
    (set (map :name author-list))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
     death-year (str author-name " (" birth-year " - " death-year ")")
     birth-year (str author-name " (" birth-year " - )")
     :else author-name)))

(defn authors->string [authors]
    (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [authors (authors->string (:authors book))
        title (:title book)]
    (apply str (interpose ", written by " [title authors]))))

(defn books->string [books]
  (let [bookstrings (apply str (map book->string books))]
    (cond
     (== (count books) 1) (str "1 book. " bookstrings ".")
     (< 1 (count books)) (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
     :else "No books.")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
