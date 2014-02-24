(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)] (Math/pow sum sum)))

(defn spiff [v] (+ (v 0) (v 2)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [[x _ y]] (+ x y))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[leftX bottomY] [rightX topY]]]
  (- rightX leftX))

(defn height  [[[leftX bottomY] [rightX topY]]]
  (- topY bottomY))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (apply * ((juxt height width) rectangle)))

(defn contains-point?
  [[[leftX bottomY] [rightX topY]] [pointX pointY]]
  (and (<= leftX pointX rightX)
       (<= bottomY pointY topY)))

(defn contains-rectangle? [outer inner]
  (every? (partial contains-point? outer) inner))

(defn title-length [{title :title}]
  (count title))

(defn author-count [{authors :authors}]
  (count authors))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [{authors :authors :as book} new-author]
  (assoc book :authors (conj authors new-author)))

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
  ((if (contains? a-set elem) disj conj) a-set elem))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [{authors :authors :as book}]
  (assoc book :authors (set authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [{:keys [name, birth-year, death-year]}]
  (str name
       (if (or birth-year death-year)
         (str " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [{:keys [title, authors]}]
  (str title ", written by " (authors->string authors)))

(defn books->string [books]
  (let [book-count (count books)]
    (cond (zero? book-count) "No books."
          (= 1 book-count) (str "1 book. " (book->string (first books)) ".")
          :else (str book-count " books. "
                     (apply str (interpose ". " (map book->string books)))
                     "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
