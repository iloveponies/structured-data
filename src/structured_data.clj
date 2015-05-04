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
  (= (height rectangle) (width rectangle) ))

(defn area [rectangle]
  (* (height rectangle) (width rectangle) ))

(defn contains-point? [rectangle point]
 (let [[px py] point
       [[x1 y1] [x2 y2]] rectangle]
   (and (<= x1 px x2) (<= y1 py y2))))


(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (conj (get book :authors) new-author)]
    (assoc book :authors authors)
  ))

(defn alive? [author]
 (= nil (get author :death-year)))

(defn element-lengths [collection]
 (map count collection))

(defn second-elements [collection]
 (let [foo (fn [x] (get x 1))]
  (map foo collection)))

(defn titles [books]
  (let [get-title (fn [x] (get x :title))]
    (map get-title books)))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
 (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [authors (set (get book :authors))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)]
    (if (nil? birth)
      (str author-name)
      (str author-name " (" birth " - " death ")" ))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let[book-name (get book :title)
       authors (authors->string (get book :authors))]
  (str book-name ", written by " authors)))

(defn books->string [books]
  (let [amount (count books)]
  (str (apply str (cond
        (= amount 0) "No books"
        (= amount 1) "1 book. "
        :else (str amount " books. "))
       (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (let [foo (fn [x] (has-author? x author))]
    (filter foo books)))

(defn author-by-name [name authors]
  (let [foo (fn [x] (= name (get x :name)))]
    (first (filter foo authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
