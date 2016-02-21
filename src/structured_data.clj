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
    (if (< x1 x2)
      (- x2 x1)
      (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (< y1 y2)
      (- y2 y1)
      (- y1 y2))))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[xy zw] inner]
    (and (contains-point? outer xy) (contains-point? outer zw))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [current_authors (:authors book)]
    (assoc book :authors (conj current_authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secele (fn [c] (get c 1))]
    (map secele collection)))

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
  (if (< (count (set a-seq)) (count a-seq))
    true
    false))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (if (= name nil)
      ""
      (if (= birth-year nil)
        name
        (if (= death-year nil)
          (str name " (" birth-year " - )")
          (str name " (" birth-year " - " death-year ")"))))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (if (= "" (authors->string (:authors book)))
    (:title book)
    (str (:title book) ", written by " (authors->string (:authors book)))))

(defn books->string [books]
  (if (= 0 (count books))
    "No books."
    (if (= 1 (count books))
      (str "1 book. " (book->string (get books 0)) ".")
      (let [lazybitch (apply str (interpose ". " (map book->string books)))]
        (str (count books) " books. " lazybitch ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (filter alive? (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
