(ns structured-data)
;Custom Defs
(def lewis {:name "Lewis Carroll" :birth-year 1832 :death-year 1898})
(def alice {:title "Through the Looking-Glass" :authors [lewis]})

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

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
  (let [[[tx ty] [bx by]] rectangle]
    (- bx tx)))

(defn height [rectangle]
  (let [[[tx ty] [bx by]] rectangle]
    (- by ty)))

(defn square? [rectangle]
  (= (height rectangle)(width rectangle)))

(defn area [rectangle]
  (* (height rectangle)(width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[tx ty] [bx by]] rectangle]
    (let [[x y] point]
      (and (>= bx x tx)(>= by y ty)))))

(defn contains-rectangle? [outer inner]
  (let [[a b] inner]
    (and (contains-point? outer a) (contains-point? outer b))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sc (fn [x] (get x 1))]
    (map sc collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors (map old-book->new-book books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
  (if birth-year
    (if death-year
      (str name " (" birth-year " - " death-year ")")
      (str name " (" birth-year " - )"))
    name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
(let [allbks (apply str (interpose ". " (map book->string books)))]
   (if (<= 1 (count books))
    (if (= 1 (count books))
      (str "1 book. " allbks ".")
      (str (count books) " books. " allbks "."))
    "No books.")))

(defn books-by-author [author books]
  (filter (fn [v] (has-author? v author)) books))

(defn author-by-name [name authors]
  (if (= authors []) nil
  (first (filter (fn [x] (= name (:name x))) authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%