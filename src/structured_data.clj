(ns structured-data)

(defn do-a-thing [x]
  (let [thing (+ x x)]
    (Math/pow thing thing)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[first _ third]]
  (+ first third))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _] [x2 _]]]
  (- x2 x1))

(defn height [[[_ y1] [_ y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xt yt] point]
    (and (<= x1 xt x2)
         (<= y1 yt y2))))

(defn contains-rectangle? [outer inner]
  (let [[x y] inner]
    (and (contains-point? outer x)
         (contains-point? outer y))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

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
  (not= (count a-seq)
        (count (set a-seq))))

(defn old-book->new-book [book]
  (let [set-authors (set (:authors book))]
    (assoc book :authors set-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map #(:authors %) books)))

(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  #_(set (apply concat (map author-names books)))
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (cond (:death-year author) (str " (" (:birth-year author) " - " (:death-year author) ")")
                    (:birth-year author) (str " (" (:birth-year author) " - )")
                    :else "")]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [book-count (count books)
        preamble (cond (= 0 book-count) "No books."
                       (= 1 book-count) "1 book."
                       :else (str book-count " books."))
        book-strings (map book->string books)
        books-with-punct (map #(str % ".") book-strings)
        body-seq (cons preamble books-with-punct)]
    (apply str (interpose " " body-seq))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
