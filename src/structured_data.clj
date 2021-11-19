(ns structured-data)

(defn do-a-thing [x]
  (let [doubled (+ x x)]
    (Math/pow doubled doubled)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[f _ t]]
  (+ f t))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _] [x2 _]]]
  (Math/abs (- x2 x1)))

(defn height [[[_ y1] [_ y2]]]
  (Math/abs (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [[[x1 _] [_ y1]] [[x2 _] [_ y2]]]
  (and (<= x1 x2) (<= y2 y1)))

(defn title-length [{:keys [title]}]
  (count title))

(defn author-count [{:keys [authors]}]
  (count authors))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (update book :authors (fn [v] (conj v new-author))))

(defn alive? [{:keys [death-year]}]
  (nil? death-year))

(defn element-lengths [collection]
  (into [] (map count) collection))

(defn second-elements [collection]
  (into [] (map second) collection))

(defn titles [books]
  (into [] (map :title) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (update book :authors (fn [v] (set v))))

(defn has-author? [{:keys [authors]} author]
  (not (nil? ((set authors) author))))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [{:keys [name birth-year death-year]}]
  (cond 
    death-year
    (str name " (" birth-year " - " death-year ")")
    birth-year
    (str name " (" birth-year " - )")
    :else
    name))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [{:keys [title authors]}]
  (str title ", written by " (authors->string authors)))

(defn books->string [books]
  (let [count-book (count books)
        count-book-string (cond (= count-book 0) "No books."
                                (= count-book 1) (str count-book " book. ")
                                :else (str count-book " books. "))
        books-strings (interpose ", " (map book->string books))
        books-string (when-not (empty? books-strings) (apply str (conj (into [] books-strings) ".")))]
    (str count-book-string books-string)))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (when-let [author (first (filter #(-> % :name (= name)) authors))]
    author))

(defn living-authors [authors]
  (filter #(-> % :death-year nil?) authors))

(defn has-a-living-author? [{:keys [authors]}]
  (<= 1 (count (living-authors authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
