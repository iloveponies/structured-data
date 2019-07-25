(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

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
  (let [[[bl-x bl-y] [tr-x tr-y]] rectangle]
    (- tr-x bl-x)))

(defn height [rectangle]
  (let [[[bl-x bl-y] [tr-x tr-y]] rectangle]
    (- tr-y bl-y)))

(defn square? [rectangle]
  (== (width rectangle)
      (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[bl-x bl-y] [tr-x tr-y]] rectangle
        [px py] point]
    (and (<= bl-x px tr-x)
         (<= bl-y py tr-y))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [book-authors (:authors book)]
    (assoc book :authors (conj book-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [v] (get v 1)) collection))

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
  (not (= (count a-seq)
          (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-books
        (fn [book] (:authors book))]
    (apply clojure.set/union (map author-books books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        b-year (get author :birth-year "")
        d-year (get author :death-year "")]
    (cond
     (not= d-year "") (str name " (" b-year " - " d-year ")")
     (not= b-year "") (str name " (" b-year " - )")
     :else name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)]
    (cond
     (== n 0) "No books."
     (== n 1) (str "1 book. " (book->string (first books)) ".")
     :else (str n " books. "
                (apply str (interpose ". " (map book->string books))) "."))))

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
