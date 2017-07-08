(ns structured-data)

(defn do-a-thing [x]
  (let [tx (+ x x)]
    (Math/pow tx tx))
)

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
  (let [[[x1, y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1, y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1, y1] [x2 y2]] rectangle
        [x3,y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2))
    true
    false)))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (if (and (contains-point? outer bottom-left) (contains-point? outer top-right))
    true
    false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
  true
  false))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author))
)

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)))

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
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books))  
)

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth (:birth-year author)
        death (:death-year author)
        name  (:name author)]
        (cond
          (and (nil? death) (nil? birth)) (str name)
          (nil? death) (str name " (" birth " - )")
          :else (str name " (" birth " - " death ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [name (book :title)
        authors (authors->string (book :authors))]
        (str name ", written by " authors)))

(defn books->string [books]
  (let [c (count (map book->string books))
        books (apply str (interpose ". " (map book->string books)))]
        (cond
          (== 0 c) (str "No books.")
          (== 1 c) (str "1 book. " books ".")
          :else (str c " books. " books "."))))

(defn books-by-author [author books]
  (let [helper (fn [book] (has-author? book author))]
  (filter helper books)))

(defn author-by-name [name authors]
  (let [helper (fn [author] (= name (author :name)))]
    (first(filter helper authors))))

(defn living-authors [authors]
  (let [helper (fn [author] (alive? author))]
    (filter helper authors)
  ))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
