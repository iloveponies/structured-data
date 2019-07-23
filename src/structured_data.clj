(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
 (+ (get v 0) (get v 2)))

(defn cutify [v]
 (conj v "<3"))

(defn spiff-destructuring [[x y z]]
 (+ x z))

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
  (if (== (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (== (author-count book) 1)
    false
    true))

(defn add-author [book new-author]
 (let [orig (get book :authors)
    newg (assoc orig (count orig) new-author)]
    (assoc book :authors newg)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (seq (let [fun (fn [x] (get x 1))]
    (map fun collection))))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond (apply <= a-seq) true
        (apply >= a-seq) true
        :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond (contains? a-set elem) (disj a-set elem)
        :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [newauths (set (get book :authors))]
    (assoc book :authors newauths)))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [namee (:name author)
        year1 (:birth-year author)
        year2 (:death-year author)]
    (cond year2 (str namee " (" year1 " - " year2 ")")
          year1 (str namee " (" year1 " - )")
          :else namee)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [namestr (str (:title book) ", written by ")
        authorstr (authors->string (:authors book))]
    (str namestr authorstr)))

(defn books->string [books]
  (cond (< 1 (count books)) (str (count books) " books. " 
                             (apply str
                             (interpose ". " (map book->string books))) ".")
        (== 1 (count books)) (str "1 book. " (book->string (get books 0))".")
         :else "No books.")) 

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
