(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (let [v-first (get v 0)
        v-third (get v 2)]
    (+ v-first v-third)))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v-first _ v-third] v]
    (+ v-first v-third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[left-x bottom-y] (first rectangle)
        [right-x top-y]   (second rectangle)]
    (- right-x left-x)))

(defn height [rectangle]
  (let [[left-x bottom-y] (first rectangle)
        [right-x top-y]   (second rectangle)]
    (- top-y bottom-y)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[left-x bottom-y] (first rectangle)
        [right-x top-y]   (second rectangle)
        [x y]             point]
    (and
      (<= left-x x right-x)
      (<= bottom-y y top-y))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and
      (contains-point? outer bottom-left)
      (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [col] (second col))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name  (:name author)
        author-years (if (:birth-year author)
                       (str " (" (:birth-year author) " - " (:death-year author) ")")
                       nil)]
    (str author-name author-years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [count-books (count books)
          summary      (cond
                         (= 1 count-books) "1 book."
                         :else             (str count-books " books."))
          book-strings (map book->string books)]
      (str summary " " (apply str (interpose ". " book-strings)) "."))))

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
