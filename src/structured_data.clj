(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
     (Math/pow xx xx)))

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

(defn width [[[bx by] [tx ty]]]
  (- tx bx))

(defn height [[[bx by] [tx ty]]]
  (- ty by))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[bx by] [tx ty]] [x y]]
  (and (<= bx x tx) (<= by y ty)))

(defn contains-rectangle? [[[bx1 by1] [tx1 ty1]] [[bx2 by2] [tx2 ty2]]]
  (and (<= bx1 bx2) (>= tx1 tx2) (<= by1 by2) (>= ty1 ty2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1)
    true
    false))

(defn add-author [book new-author]
  (let [oauth (get book :authors)
        nauth (conj oauth new-author)
        nbook (assoc book :authors nauth)]
     nbook))

(defn alive? [author]
   (cond
     (contains? author :death-year) false
     :else true))

(defn element-lengths [collection]
    (map count collection))

(defn second-elements [collection]
  (let [get1 (fn [vect] (get vect 1))]
     (map get1 collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
     (contains? a-set elem) (disj a-set elem)
     :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (:name (authors books))))

(defn author->string [author]
  (let [n (:name author)
        dy (if (contains? author :death-year) (:death-year author) (str ""))
        by (:birth-year author)
        y (if (and (not (contains? author :death-year)) (not (contains? author :birth-year)))
            (str "")
            (str " " "(" by " - " dy ")"))]
     (str n y)))

(defn authors->string [authors]
  (let [sep ", "
        f (fn [a] (if (= sep (:name a)) (str sep) (author->string a)))]
     (apply str (map f (interpose sep authors)))))

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
