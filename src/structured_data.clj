(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[bl _] [tr _]]]
  (- tr bl))

(defn height [[[_ bl] [_ tr]]]
  (- tr bl))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and (<= x1 px x2)
       (<= y1 py y2)))

(defn contains-rectangle? [outer [i1 i2]]
  (and (contains-point? outer i1)
       (contains-point? outer i2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map
    (fn [c] (get c 1))
    collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq)
     (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [books-authors (map :authors books)]
    (apply clojure.set/union books-authors)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (contains? author :birth-year)
                (str "(" (:birth-year author) " - " (:death-year author) ")")
                "")]
    (if (= years "")
      name
      (str name " " years))))

(defn authors->string [authors]
  (let [strings (map author->string authors)]
    (apply str (interpose ", " strings))))

(defn book->string [book]
  (let [authors (authors->string (:authors book))
        title (:title book)]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [cnt (count books)
        word (if (> cnt 1)
               " books"
               " book")
        bookstr (apply str (interpose ". " (map book->string books)))]
    (if (empty? books)
      "No books."
      (str cnt word ". " (apply str bookstr ".")))))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [n (count (living-authors (:authors book)))]
    (> n 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
