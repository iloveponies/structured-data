(ns structured-data)

(defn do-a-thing [x]
  (let [sx (+ x x)]
    (Math/pow sx sx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

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
  (=  (width rectangle)
      (height rectangle)))

(defn area [rectangle]
  (*  (width rectangle)
      (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a b] point]
    (and (<= x1 a x2)
         (<= y1 b y2))))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and (contains-point? outer bl)
         (contains-point? outer tr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors
           (conj authors new-author))))

(defn alive? [author]
  (not  (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec-elem (fn [x] (get x 1))]
    (map sec-elem collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [inc? (fn [a-seq] (apply <= a-seq))
        dec? (fn [a-seq] (apply >= a-seq))]
    (or (inc? a-seq) (dec? a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not= (count a-seq) (count a-set))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)]
    (assoc book :authors (set old-authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [book-authors (map :authors books)]
    (apply clojure.set/union book-authors)))


(defn all-author-names [books]
  (set  (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        life-string  (if (contains? author :birth-year)
                       (str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str name life-string)))

(defn authors->string [authors]
  (let [author-names (map author->string authors)]
    (apply str (interpose ", " author-names))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors  book))))

(defn books->string [books]
  (let [book-size (count books)
        intro-str (cond (= book-size 0) "No books."
                    (= book-size 1) "1 book. "
                    :else (str book-size " books. "))
        book-strings (map book->string books)
        body-str (apply str(interpose ". " book-strings))
        closing-str (if (empty? books) "" ".")]
    (str intro-str body-str closing-str)))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first  (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not  (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
