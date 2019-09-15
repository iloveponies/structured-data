(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

;; next function checks if v is a vector shorter than 3 elements, returning
;; nil in this case

(defn spiff [v]
  (if (< (count v) 3)
    nil
    (let [a (get v 0)
        b (get v 2)]
    (+ a b))))


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))


(defn point [x y]
  [x y])


(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))


(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1)
        (- y2 y1))))


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        base (- x2 x1)
        alt (- y2 y1)]
    (* base alt)))



(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2)
         (<= y1 yp y2))))


(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1)
         (contains-point? outer point2))))


(defn title-length [book]
  (let [titolo (:title book)]
    (count titolo)))


(defn author-count [book]
  (let [autori (:authors book)]
    (count autori)))


(defn multiple-authors? [book]
  (> (author-count book) 1))


(defn add-author [book new-author]
  (let [original (:authors book)
        new (conj original new-author)]
    (assoc book :authors new)))


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [secondo (fn [coll] (get coll 1))]
    (map secondo collection)))


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))


(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (let [insieme (set a-seq)]
    (< (count insieme) (count a-seq))))


(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))


(defn has-author? [book author]
  (let [autori (:authors book)]
    (contains? autori author)))

(defn authors [books]
  (set (apply concat (map :authors books))))


(defn all-author-names [books]
  (let [autori (authors books)]
    (set (map :name autori))))


(defn author->string [author]
  (let [nome (:name author)
        data-nasc (:birth-year author)
        data-morte (:death-year author)]
    (if (nil? data-nasc)
      (str nome)
      (if (nil? data-morte)
        (str nome " (" data-nasc " - )")
        (str nome " (" data-nasc " - " data-morte ")")))))


(defn authors->string [authors]
  (let [autori (map author->string authors)]
    (apply str (interpose ", " autori))))


(defn book->string [book]
  (let [aut-str (authors->string (:authors book))]
    (str (:title book) ", written by " aut-str)))


(defn books->string [books]
  (if (empty? books)
    "No books."
    (if (= (count books) 1)
      (str "1 book. " (book->string (first books)) ".")
      (str (apply str (interpose ". " (cons (str (count books) " books") (map book->string books)))) "."))))


(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))


(defn author-by-name [name authors]
  (let [matches (filter (fn [x] (= (:name x) name)) authors)]
    (if (empty? matches)
      nil
      (first matches))))


(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))


(defn has-a-living-author? [book]
  (let [autori (:authors book)
        autori-vivi (filter (fn [x] (alive? x)) autori)]
    (not (empty? autori-vivi))))


(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))


; %________%
