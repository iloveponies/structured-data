(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
	(Math/pow x x)))

(defn spiff [v]
  let [a (get v 0)
	b (get v 2)]
	(+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x b y ] v] (+ x y)))

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
  (let [[[x1 y1] [x2 y2]]
    rectangle] (= (- x2 x1)
    (- y2 y1))))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]]
     rectangle [x y] point]
     (and (<= x1 x x2)
     (<= y1 y y2)) ))

(defn contains-rectangle? [outer inner]
 (let [[x y] inner]
    (and (contains-point? outer x)
         (contains-point? outer y))))

(defn title-length [book]
   (let[ tbook (:title book)]
     (count tbook)))

(defn author-count [book]
 (count (get book :authors)))


(defn multiple-authors? [book]
   (> (author-count book) 1))


(defn add-author [book new-author]
  (let [authors (conj (get book :authors) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
   (nil? (get author :death-year)))

(defn element-lengths [collection]
  (let [el (fn [x] (count x))]
     (map el collection) ))

(defn second-elements [collection]
  (let [mg (fn [x] (second (seq x)))]
     (map mg collection)))

(defn titles [books]
   (let [name (fn [book] (get book :title))]
    (map name books)))

(defn monotonic? [a-seq]
 (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
   (let [b  (repeat n  "*")]
   (apply str b)) )

(defn toggle [a-set elem]
   (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [b (count a-seq) c (count (set a-seq))]
  (> b c) )  )

(defn old-book->new-book [book]
 (assoc book :authors
  (set (:authors book))) )

(defn has-author? [book author]
   (contains? (:authors book)
    author))

(defn authors [books]
  (let [authors (fn [book] (get book :authors))]
    (apply set/union (map authors books))))


(defn all-author-names [books]
 (let [author-name (fn [author] (get author :name))]
    (set (map author-name (authors books)))))

(defn author->string [author]
  (let [byear (:birth-year author)
    dyear (:death-year author) name
    (:name author) years (str " " "("
     byear " " "-" " " dyear ")")]
    (if byear (str name years) name) ))

(defn authors->string [authors]
  (let [authorAsString (fn [author] (author->string author))]
    (apply str (interpose ", " (map authorAsString authors)))))

(defn book->string [book]
   (str (get book :title) ", written by " (authors->string (get book :authors))))


(defn books->string [books]
  (if (empty? books)
    (str "No books.")

    (str (if (= 1 (count books))
      (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
      (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))))


(defn books-by-author [author books]
  (filter (fn [books] (contains?
  (:authors books) author)) books))

(defn author-by-name [name authors]
   (let [x (filter (fn [authors]
  (= (:name authors) name)) authors)y
  (first (map :name x))] (if (= x ())
  nil (first x) )))

(defn living-authors [authors]
  (filter (fn [authors] (not (contains?
  authors :death-year ))) authors ))

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
