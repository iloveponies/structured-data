(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (< (count v) 3)
    nil
    (+ (get v 0) (get v 2))))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (< (count v) 3)
    nil
    (let [[st nd rd] v]
    (+ st rd))
         ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and (<= x1 px x2)
       (<= y1 py y2)))

(defn contains-rectangle? [outer [innerX innerY]]
  (and (contains-point? outer innerX)
       (contains-point? outer innerY)))

(defn title-length [book]
  (count(:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [uauthor (conj (:authors book) new-author)]
    (assoc book :authors uauthor)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [nd (fn [v] (get v 1))]
+    (map nd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [a-set (set (:authors book))]
    (assoc book :authors a-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [book-authors (fn [book] (:authors book))]
    (apply clojure.set/union (map book-authors books))))

(defn all-author-names [books]
  (let [author-name (fn [author] (:name author))]
    (set (map author-name (authors books)))))

(defn author->string [author]
  (let [a-name (:name author)]
     (let [a-birth-year (:birth-year author)]
       (let [a-death-year (:death-year author)]
         (str a-name (if (not (nil? a-birth-year)) (str " (" a-birth-year " - " a-death-year ")")))))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book_count (count books)]
     (if (= book_count 0) "No books."
       (str book_count (if (= book_count 1) " book. " " books. ")
              (apply str (interpose ". " (map book->string books))) "."))))



(defn books-by-author [author books]
  (let [a-books (fn [book] (has-author? book author))]
    (filter a-books books)))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
