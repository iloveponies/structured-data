(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)] 
   (Math/pow xx xx)))

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
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x3 y3]]
  (and (<= y1 y3 y2) (<= x1 x3 x2)))

(defn contains-rectangle? [outer [point1 point2]]
  (and (contains-point? outer point1) (contains-point? outer point2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [uudet (:authors book)]
    (assoc book :authors (conj uudet new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [tokat (fn [vek] (get vek 1))]
    (map tokat collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
 (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [kirjailijat (:authors book)]
    (assoc book :authors (set kirjailijat))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
    (cond
      (not (contains? author :birth-year)) (:name author)
      (contains? author :death-year)
           (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
      (contains? author :birth-year)
           (str (:name author) " (" (:birth-year author) " - )")))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [nimi (:title book)
        teki (authors->string (:authors book))]
    (str nimi ", written by " teki)))

(defn books->string [books]
    (cond
      (== (count books) 0) "No books."
      (== (count books) 1) (str "1 book. " (apply str (interpose ", " (map book->string books))) ".")
      :else (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [oikea (filter (fn [x] (= (:name x) name)) authors)]
    (if (empty? oikea)
      nil
      (first oikea))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
