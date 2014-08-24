(ns structured-data)

(defn do-a-thing [x]
  (let [times2 (+ x x)]
    (Math/pow times2 times2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first _ third] v]
    (+ first third)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[[x1 y1] [x2 y2]] [px py]] [rectangle point]]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and (contains-point? outer inner-bottom-left)
         (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)]
    (let [new-authors (conj old-authors new-author)]
      (assoc book :authors new-authors))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map #(count %1) collection))

(defn second-elements [collection]
  (map #(get %1 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors-seq (:authors book)]
    (assoc book :authors (set authors-seq))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-sets (map :authors books)]
    (apply clojure.set/union author-sets)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)]
    (let [birth-year (:birth-year author)]
      (let [death-year (or (:death-year author) "")]
        (let [lived (if (not birth-year)
                      (str "")
                      (str " (" birth-year " - " death-year ")"))]
          (str author-name lived))))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count-sentence
        (cond
          (empty? books) (str "No books")
          (== (count books) 1) "1 book"
          :else (str (count books) " books"))]
    (let [books-string (map book->string books)]
      (str (apply str (interpose ". " (concat (list book-count-sentence) books-string))) "."))))


(defn books-by-author [author books]
  (filter #(has-author? %1 author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %1) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
