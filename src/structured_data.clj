(ns structured-data)

(defn do-a-thing [x]
  (let [x1 (+ x x)]
    (Math/pow x1 x1)))

(defn spiff [v]
  (if (< (count v) 3)
    nill
    (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (< (count v) 3)
    nil
    (let [[x y z] v]
      (+ x z))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[a b] [c d]] rectangle]
    (Math/abs (- c a))))

(defn height [rectangle]
  (let [[[a b] [c d]] rectangle]
    (Math/abs (- d b))))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[a b] [c d]] rectangle [m n] point]
    (if (and (<= a m c) (<= b n d))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (if (and (contains-point? outer point1)
          (contains-point? outer point2))
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
  (let [au (:authors book)
        new-au (conj au new-author)]
    (assoc book :authors new-au)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
   (map :title books))

(defn monotonic? [a-seq]
 (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= ((count (set a-seq)) (count a-seq))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [ name1 (:name author)
         date1 (cond
                   (contains? author :birth-year)
                   (str " (" (:birth-year author) " - )")
                   (contains? author :death-year)
                   (str " (" (:birth-year author) " - " (:death-year author) ")")
                   :else "")]
    (str name1 date1)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)
         sum1  (cond
                 (== 0 n) "No books"
                 (== 1 n) "1 book"
                 :else (str n " books"))
         sum2  (cond 
                    (< 0 n)
                    (apply str ". "(interpose ". " (map book->string books)))
                    "")]
    (str sum1 sum2 ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (#(alive? %)) authors))

(defn has-a-living-author? [book]
  (let [live (filter #(alive? %) (:authors book))]
    (not (empty? live))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
