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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [x (width rectangle)
        y (height rectangle)]
    (if (== x y ) true false)))

(defn area [rectangle]
  (let [x (width rectangle)
        y (height rectangle)]
    (* x y)))


(defn contains-point? [rectangle point]
  (let [[[[x1 y1] [x2 y2]] [x y]] [rectangle point]]
   (if (and (<= x1 x x2) (<= y1 y y2)) true false)))


(defn contains-rectangle? [outer inner]
  (let [[[x y] [x1 x2]] [outer inner]]
    (if (and (contains-point? [x y] x1) (contains-point? [x y] x2)) true false)))


(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (let [[x] [(:authors book)]]
  (count x)))


(defn multiple-authors? [book]
  (let [x (author-count book)]
    (if (> x 1) true false)))


(defn add-author [book new-author]
  (let [x (conj (:authors book) new-author)
        new (assoc book :authors x)]
    new))


(defn alive? [author]
  (if (contains? author :death-year) false true))


(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [x] (get x 1))]
    (map helper collection)))


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (let [add (conj a-set elem)
        re (disj a-set elem)]
    (if (contains? a-set elem) re add)))


(defn contains-duplicates? [a-seq]
  (let [x (set a-seq)]
    (if (= (count x) (count a-seq)) false true)))

(defn old-book->new-book [book]
  (let [authors (set (concat (:authors book)))
        new (assoc book :authors authors)]
    new))


(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))


(defn authors [books]
  (let [helper (fn [book] (map :authors book))]
   (apply clojure.set/union (helper books))))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name-y (:name author)
        birth-death (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (if (contains? author :birth-year) (str name-y birth-death) (str name-y))))


(defn authors->string [authors]
  (let [helper (fn [author] (interpose ", " author))]
    (apply str (helper (map author->string authors)))))



(defn book->string [book]
  (let [title (:title book)
       written (str ", written by " (authors->string (:authors book)))]
  (str title written)))


(defn books->string [books]
  (let [helper (fn [book] (interpose ". " book))
        counter (if (empty? books) "No books."
                  (if (> (count books) 1) (str (count books) " books. ") (str "1 book. ")))]
    (str counter (apply str (helper (map book->string books)))
         (if (empty? books) "" "."))))


(defn books-by-author [author books]
    (filter (fn [book] (has-author? book author)) books))



(defn author-by-name [name authors]
 (first (filter (fn [author] (= (:name author) name)) authors)))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
