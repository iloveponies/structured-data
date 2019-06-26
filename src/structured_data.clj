(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (* x 2)]
    (Math/pow x+x x+x)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ b]]
  (+ a b))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= y1 y y2) (<= x1 x x2)))

(defn contains-rectangle? [outer [bl tr]]
  (and (contains-point? outer bl)
       (contains-point? outer tr)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (or (:authors book) [])
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  ((complement contains?) author :death-year))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [b-seq (rest a-seq)
        asc-seq (map <= a-seq b-seq)
        dsc-seq (map >= a-seq b-seq)]
    (or (every? true? asc-seq)
        (every? true? dsc-seq))))

(defn stars [n]
  (apply str (apply repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq)
        (count (set a-seq))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (-> book
      :authors
      (contains? author)))

(defn authors [books]
  (->> books
       (map :authors)
       (apply clojure.set/union)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [{author-name :name :keys [birth-year death-year]}]
  (cond death-year (str author-name " (" birth-year " - " death-year ")")
        birth-year (str author-name " (" birth-year " - )")
        :else author-name))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [{:keys [title authors]}]
  (str title ", written by " (authors->string authors)))

(defn books->string [books]
  (letfn [(count-str [cnt]
                     (cond (zero? cnt) "No books."
                           (= cnt 1) "1 book. "
                           :else (str cnt " books. ")))]
    (let [cnt (count books)
          cnt-str (count-str cnt)
          b-str (map book->string books)]
      (if (zero? cnt)
        cnt-str
        (str cnt-str (apply str (interpose ". " b-str)) ".")))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [{authors :authors}]
  ((complement empty?) (living-authors authors)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
