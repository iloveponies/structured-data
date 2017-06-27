(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))

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
  (== (width rectangle)
      (height rectangle)))

(defn area [rectangle]
   (* (height rectangle)
      (width rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [xp yp]]
  (and (<= x1 xp x2)
       (<= y1 yp y2)))

(defn contains-rectangle? [outer [inner1 inner2]]
  (and (contains-point? outer inner1)
       (contains-point? outer inner2)))

(defn title-length [book]
  (count (book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original (book :authors)
        new      (conj original new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [length
         (fn [collection] (count collection))]
    (map length collection)))

(defn second-elements [collection]
  (let [second
         (fn [collection] (get collection 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (let [helper
         (fn [n] (repeat n "*"))]
    (apply str (helper n))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq)
      (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authorset
         (get book :authors)]
    (assoc book :authors (set authorset))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [all
         (map :authors books)]
    (apply clojure.set/union all)))

(defn all-author-names [books]
  (clojure.set/union (set (map :name (authors books)))))

(defn author->string [author]
  (let [
        name
         (fn [author] (str (:name author)))
        year
         (fn [author] (if (contains? author :birth-year)
                          (str " (" (:birth-year author) " - " (:death-year author) ")")))]
     (str (name author) (year author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
    (let [
        title
         (fn [book] (str (:title book)))
        written
         (fn [book] (str ", written by " (authors->string (:authors book))))]
     (str (title book) (written book))))

(defn books->string [books]
  (str (cond
  (== 1 (count books)) (str (count books) " book. ")
  (<= 2 (count books)) (str (count books) " books. ")
  :else "No books") (apply str (interpose ", " (map book->string books))) "."))

(defn books-by-author [author books]
  (filter (fn [books] (has-author? (clojure.set/union books) author)) books))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
