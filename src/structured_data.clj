(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[a1 b1][a2 b2]] rectangle [x y] point]
    (and (<= a1 x a2) (<= b1 y b2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (let [t (:title book)]
    (count t)))

(defn author-count [book]
  (let [a (:authors book)]
    (count a)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [n (conj (:authors book) new-author)]
    (assoc book :authors n)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn stars [n]
  (apply str (repeat n "*")))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [x (:authors book)]
  (assoc book :authors (set x))))

(defn has-author? [book author]
  (let [auth-list (:authors book)]
    (if (contains? auth-list author)
      true
      false)))

(defn authors [books]
  (let [auth (fn [bk] (:authors bk))]
   (apply clojure.set/union (map auth books))))


(defn all-author-names [books]
  (let [auth (authors books)]
    (set (map :name auth))))


(defn author->string [author]
  (let [name (:name author)
        years (cond
               (not (nil? (:death-year author)))
                (apply str " " "(" (:birth-year author) " - " (:death-year author) ")")
               (not (nil? (:birth-year author)))
                (apply str " " "(" (:birth-year author) " - " ")")
               :else
                nil)]
        (if
          (nil? years)
          (str name)
          (str name years))))


(defn authors->string [authors]
  (let [a (fn [b] (author->string b)) auth (map a authors)]
  (apply str (interpose ", " auth))))

(defn book->string [book]
    (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (let [num (count books)]
    (cond (< num 1) "No books."
          (< num 2)  (str num " book. " (book->string (get books 0)) "." )
          :else
              (let [bookstr (map book->string books)]
              (str num " books. " (apply str (interpose ". " bookstr)) "." )))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [auth (filter (fn [x] (= name (:name x))) authors)]
   (if (= auth '())
      nil
      (first auth))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

