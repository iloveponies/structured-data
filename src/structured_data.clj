(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))


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

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
     (if (= (width rectangle) (height rectangle)) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
     (* (width rectangle) (height rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x3 y3] point]
     (if (and (<= x1 x3 x2) (<= y1 y3 y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x3 y3] [x4 y4]] inner]
     (if (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4])) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map (fn [x]
         (count x)) collection))


(defn second-elements [collection]
  (map (fn [x]
         (get x 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
   (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (book :authors) author) true false))

(defn authors [books]
  (apply clojure.set/union  (map :authors books)))

(defn all-author-names [books]
  (set (apply clojure.set/union (map (fn [x] (map :name (:authors x))) books))))

(defn author->string [author]
  (cond (and (alive? author) (contains? author :birth-year)) (apply str #{(str (:name author) " (" (:birth-year author) " - )")})
        (not (alive? author)) (apply str #{(str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")})
        :else (apply str #{(str (:name author))})))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str [(:title book) ", written by " (authors->string (:authors book))]))

(defn books->string [books]
   (let [nb-book (count books)
        nb-book-str (cond
                  (= nb-book 0) "No books"
                  (= nb-book 1) "1 book. "
                  :else (str nb-book " books. "))
        books-str (apply str (interpose ". " (map book->string books)))]
        (str nb-book-str books-str ".")
))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
 (first (filter (fn [author]
(= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter (fn [x] (if (has-a-living-author? x) true false)) books))

; %________%

