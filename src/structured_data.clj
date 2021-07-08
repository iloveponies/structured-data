(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (cond
   (get v 2)   (+ (get v 0) (get v 2))
   :else       nil))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (cond
   (get v 2) (let [[x y z] v]
               (+ x z))
   :else     nil))

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
    (if(zero?(-(- x1 y1) (- x2 y2))) true false)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [px py] point]
    (if(and (<= x1 px x2) (<= y1 py y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[inner1 inner2] inner]
    (if(and (contains-point? outer inner1) (contains-point? outer inner2)) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if(contains? author :death-year) false true))

(defn element-lengths [collection]
  (map (fn [collection] (count collection)) collection))

(defn second-elements [collection]
  (map (fn [c] (second c)) collection))

(defn titles [books]
  (map (fn [c] (:title c)) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(not(contains? a-set elem)) (conj a-set elem) (disj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map (fn [x] (:authors x)) books))))

(defn all-author-names [books]
  (let [author-full (authors books)]
  (set (map :name author-full))))

(defn author->string [author]
  (cond
   (contains? author :death-year) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")" )
   (contains? author :birth-year) (str (:name author) " (" (:birth-year author) " - )")
   :else                          (str (:name author))))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [c
  (cond
   (empty? books) (str "No books")
   (= (count books) 1) (str "1 book. ")
   :else               (str (count books) " books. "))]
   (str c (apply str (interpose ". " (map book->string books)))".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [l (map (fn [author] (if (= (:name author) name) author)) authors)]
  (if (zero? (count l)) nil (first (filter (fn [x] (not= x nil)) l)))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (if (>= (count (living-authors (:authors book))) 1) true false))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))
