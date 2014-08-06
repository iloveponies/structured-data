(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

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
  (let [[[x1] [x2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (if (= h w)
      true
      false)))

(defn area [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (* h w)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (if (and (>= x2 p1 x1) (>= y2 p2 y1))
    true
    false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer [x1 y1])
             (contains-point? outer [x2 y2]))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-sec (fn [collection] (get collection 1))]
    (map get-sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (apply <= a-seq)
    true
    (if (apply >= a-seq)
      true
      false)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seqcount (count a-seq)
        setcount (count (set a-seq))]
    (if (= seqcount setcount)
      false
      true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (let [author-names (fn [book] (:authors book))]
    (apply clojure.set/union (map author-names books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [aname (:name author)
        ayear (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (if (:birth-year author)
      (str aname " " ayear)
      aname)))

(defn authors->string [authors]
  (let [get-authors (fn [author] (author->string author))]
    (apply str (interpose ", " (map get-authors authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bstring (map book->string books)]
   (if (> (count bstring) 1)
     (str (count bstring) " books. " (apply str (interpose ". " bstring)) ".")
     (if (= (count bstring) 1)
       (str (count bstring) " book. " (apply str bstring) ".")
       "No books."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
