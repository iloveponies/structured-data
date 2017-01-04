(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
    (Math/pow x+x x+x)))

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
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs(- y1 y2))))

(defn square? [rectangle]
  (let [h (height rectangle)
  w (width rectangle)]
    (== h w)))

(defn area [rectangle]
  (let [h (height rectangle)
  w (width rectangle)]
    (* h w)))

(defn contains-point? [rectangle point]
  (let [
    [[rx1 ry1] [rx2 ry2]] rectangle
    [px py] point]
      (and (<= rx1 px rx2) (<= ry1 py ry2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner ]
    (and (contains-point? outer [x1 y1])
      (contains-point? outer [x2 y1])
      (contains-point? outer [x1 y2])
      (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1 ))

(defn add-author [book new-author]
  (let [a (:authors book)
  a (conj a new-author)]
    (assoc book :authors a)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-sec (fn [v]
      (get v 1))]
      (map get-sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count (set a-seq)) (count a-seq)) false true))

(defn old-book->new-book [book]
  (let [a (set (:authors book))]
  (assoc book :authors a)))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

(defn authors [books]
  (let [get-a (fn [book] (:authors book))]
  (set (apply clojure.set/union (map get-a books)))))

(defn all-author-names [books]
  (let [get-n (fn [author] (:name author))
  authors (authors books)]
  (set (map get-n authors))))

(defn author->string [author]
  (let [name (:name author)
  [by dy] [(:birth-year author) (:death-year author)]
  name-with-years (str name " (" by " - " dy ")")]
  (if (contains? author :birth-year) name-with-years name)))

(defn authors->string [authors]
  (let [strings (interpose ", " (map author->string authors))]
    (apply str strings)))

(defn book->string [book]
  (let [authors (:authors book)
  authors (authors->string authors)
  title (:title book)]
  (str title ", written by " authors)))

(defn books->string [books]
  (cond
    (== (count books) 0) "No books."
    (== (count books) 1) (str "1 book. " (book->string (first books)) ".")
    :else (let [sbooks (interpose ". " (map book->string books))
          sbooks (apply str sbooks)]
            (str (count books) " books. " sbooks "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [name-match (fn [author] (= name (:name author)))]
  (first(filter name-match authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
