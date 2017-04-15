(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (let [sum (+ (get v 0) (get v 2))]
    sum))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y] [(get v 0) (get v 2)]]
  (+ x y)))

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
  (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[p1 p2] point]
      (if (and (<= x1 p1 x2) (<= y1 p2 y2)) true false))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
  (if (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2])) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [orig (get book :authors)
        new  (conj orig new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (let [le (fn [x] (count x))]
    (map le collection)))

(defn second-elements [collection]
  (let [abc (fn [x] (get x 1))]
    (map abc collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (let [authors (set(get book :authors))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (if (contains? (get book :authors) author) true false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (get author :name)]
  (let [birth (get author :birth-year)]
  (let [death (get author :death-year)]
    (cond
     (not= nil birth death) (apply str [name " (" birth " - " death ")"])
     (not= nil birth) (apply str [name " (" birth " - )"])
     :else (str name))))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)]
  (let [authors (authors->string (get book :authors))]
    (apply str [title ", written by " authors]))))

(defn books->string [books]
  (let [num (count books)]
  (let [boo (apply str (interpose ". " (map book->string books)))]
  (cond
   (= 0 num) "No books."
   (= 1 num) (apply str ["1 book. " boo "."])
   :else (apply str [num " books. " boo "."])))))

(defn books-by-author [author books]
    (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
