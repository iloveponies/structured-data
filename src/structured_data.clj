(ns structured-data)

(defn do-a-thing [x]
  (let [c (+ x x)]
    (Math/pow c c)))

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

(defn width [param]
  (let [[[x1 y1] [x2 y2]] param]
   (- x2 x1)))

(defn height [param]
  (let [[[x1 y1] [x2 y2]] param]
   (- y2 y1)))

(defn square? [param]
  (let [[[x1 y1] [x2 y2]] param]
   (== (- x2 x1) (- y2 y1))))

(defn area [param]
  (let [[[x1 y1] [x2 y2]] param]
   (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
      (and (<= x1 x3 x2) (<= y1 y3 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
   (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [b book]
  (assoc book :authors (conj (:authors b) new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [fun (fn [x] (get x 1))]
    (map fun collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if(== (count (set a-seq)) (count a-seq))
    false
    true))

(defn author-names [book]
  (map :name (:authors book)))

(defn all-authors [book]
  (map (:authors book)))

(defn old-book->new-book [b]
  (assoc b :authors (set (:authors b))))


(defn has-author? [book author]
  (if(contains? (:authors book) author)
    true
    false))

(defn author-names [book]
  (map :name (:authors book)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (apply concat (map author-names books))))

(defn author->string [author]
  (let [{n :name, y :birth-year, dy :death-year} author]
    (if(contains? author :death-year)
      (str n " ("y " - " dy ")")
      (if(contains? author :birth-year)
        (str n " ("y " - "")")
        (str n )))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [{t :title, a :authors} book]
    (str t ", written by " (authors->string a))))

(defn books->string [books]
   (case (count books)
     0 "No books."
     1 (str (apply str "1 book. " (interpose ", " (map book->string books))) ".")
     (str (apply str (count books) " books. " (interpose ", " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn has-a-living-author? [book]
  (if(=(filter (fn [a] ( alive? a)) (:authors book))())
    false
    true))

(defn author-by-name [n authors]
  (if(=(filter (fn [a] ( = (:name a) n)) authors)())
    nil
    (first (filter (fn [a] ( = (:name a) n)) authors))))

(defn living-authors [authors]
  (filter (fn [a] ( alive? a)) authors))

(defn books-by-living-authors [books]
  (filter (fn [a] ( has-a-living-author? a)) books))

