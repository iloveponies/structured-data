(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)] (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [outer [p1 p2]]
  (and (contains-point? outer p1) (contains-point? outer p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book) new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [e] (get e 1)) collection))

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
  (let [uniques (set a-seq), distincts (count uniques), total (count a-seq)]
    (not= distincts total)))

(defn old-book->new-book [book]
  (let [old-authors (:authors book) new-authors (set old-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [authors-seq (authors books) names (map :name authors-seq)]
    (set names)))
    

(defn author->string [author]
  (cond
    (contains? author :death-year)
      (let [name (:name author) y1 (:birth-year author) y2 (:death-year author)]
        (str name " (" y1 " - " y2 ")"))
    (contains? author :birth-year)
      (let [name (:name author) y1 (:birth-year author)]
        (str name " (" y1 " - )"))
    :else
      (:name author)))    

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors ))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books), books-str (apply str (interpose ", " (map book->string books)))]
     (cond
       (= n 0) "No books."
       (= n 1) (str "1 book. " books-str ".")
       :else   (str n " books. " books-str "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book) living (living-authors authors)]
    (if (empty? living) false true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
