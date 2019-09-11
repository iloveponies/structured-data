(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third)))

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
  (let [[[bottom-x bottom-y] [top-x top-y] [point-x point-y]] (conj  rectangle point)]
    (and (<= bottom-x point-x top-x) (<= bottom-y point-y top-y))))

(defn contains-rectangle? [outer inner]
  (let [[inner-lower-left-point inner-upper-right-point] inner]
    (and (contains-point? outer inner-lower-left-point) (contains-point? outer inner-upper-right-point))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors  (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply < a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seq-count (count a-seq)]
    (not (== seq-count (count (set a-seq))))))

(defn old-book->new-book [book]
  (let [authors-as-set (set (:authors book))]
    (assoc book :authors authors-as-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-string (:name author)]
    (if (contains? author :birth-year)
      (if (contains? author :death-year)
        (str author-string " (" (:birth-year author) " - " (:death-year author) ")")
        (str author-string " (" (:birth-year author) " - )"))
      author-string)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count->string (fn [count]
                          (cond
                           (== 0 count) "No books."
                           (== 1 count) "1 book."
                           (> count 1) (str count " books.")))
        title->string (fn [book] (:title book))
        title->and->authors->string (fn [book]
                                      (str " "
                                           (title->string book)
                                           ", written by "
                                           (authors->string (:authors book))
                                           "."))]
    (str (book-count->string (count books)) (apply str (map title->and->authors->string books)))))

(defn books-by-author [author books]
  (let [author-matches? (fn [b]
                          (has-author? b author))]
    (filter author-matches? books)))

(defn author-by-name [name authors]
  (let [name-matches? (fn [author]
                        (= (:name author) name))]
    (first (filter name-matches? authors))))

(defn living-authors [authors]
  (let [is-alive? (fn [author]
                    (alive? author))]
    (filter is-alive? authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
