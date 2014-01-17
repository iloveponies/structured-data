(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (v 0) (v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x _ y]]
  (+ x y))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rect]
  (= (width rect) (height rect)))

(defn area [rect]
  (* (width rect) (height rect)))

(defn between? [a b c]
  (<= b a c))

(defn contains-point? [[[x1 y1] [x2 y2]] [x3 y3]]
  (and (between? x3 x1 x2)
       (between? y3 y1 y2)))

(defn contains-rectangle? [outer [p1 p2]]
  (and (contains-point? outer p1)
       (contains-point? outer p2)))

(defn title-length [{title :title}]
  (count title))

(defn author-count [{authors :authors}]
  (count authors))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [{authors :authors :as book} new-author]
  (let [new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(second %) collection))

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
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [{authors :authors :as book}]
  (assoc book :authors (set authors)))

(defn has-author? [{authors :authors} author]
  (contains? authors author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

;; ---------------------------------------------------------------------
(defn author->string [{:keys [name birth-year death-year]}]
  (let [tail (cond (nil? birth-year) ""
                   (nil? death-year) (str " (" birth-year " - )")
                   :else (str " (" birth-year " - " death-year ")"))]
    (str name tail)))

(defn authors->string [authors]
  (let [author-strings-seq (map author->string authors)]
    (apply str (interpose ", " author-strings-seq))))

(defn book->string [{:keys [title authors]}]
  (str title ", written by " (authors->string authors)))

(defn books->string [books]
  (let [book-count (count books)
        head (cond (zero? book-count) "No books"
                   (= book-count 1) "1 book. "
                   :else (str book-count " books. "))
        book-string-seq (interpose ". " (map book->string books))]
    (str head (apply str book-string-seq) ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
