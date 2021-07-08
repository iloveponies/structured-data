(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle)
     (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (>= x x1) (<= x x2)
       (>= y y1) (<= y y2)))

(defn contains-rectangle? [outer [p1 p2]]
  (and (contains-point? outer p1)
       (contains-point? outer p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (boolean (not (:death-year author))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (second x))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq)
      (apply <= a-seq)))

(defn stars [n]
  (let [c (repeat n "*")]
    (apply str c)))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (< (count a-set) (count a-seq))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)
        new-authors (set old-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [v (map :authors books)]
    (set (apply concat v))))

(defn all-author-names [books]
  (let [a (authors books)]
    (set (map :name a))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)
        year (cond
               death (str "(" birth " - " death ")")
               birth (str "(" birth " - )")
               :else nil)]
    (cond
      year (str name " " year)
      :else name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [n (count books)
        count-str (cond
                    (> n 1) (str n " books. ")
                    :else "1 book. ")
        books-str (apply str (interpose ". " (map book->string books)))]
    (cond
      (> n 0) (str count-str books-str ".")
      :else "No books.")))

(defn books-by-author [author books]
  (let [f (fn [x] (has-author? x author))]
    (filter f books)))

(defn author-by-name [name authors]
  (let [f (fn [x] (= (:name x) name))]
    (first (filter f authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [living (living-authors (:authors book))]
    (not (empty? living))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
