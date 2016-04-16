(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [[a b c]]
  (+ a c))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [r]
  (let [h (height r)
        w (width r)]
    (= h w)))

(defn area [r]
  (let [h (height r)
        w (width r)]
    (* h w)))

(defn between [a b x]
  (and
    (>= x a)
    (<= x b)))

(defn contains-point? [[[bottom left] [top right]] [x y]]
  (and
    (between bottom top x)
    (between left right y)))

(defn contains-rectangle? [outer [a b]]
  (let [cp (partial contains-point? outer)]
    (and
      (cp a)
      (cp b))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [{authors :authors}]
  (count authors))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [[a b]] b) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [xs]
  (let [ys (sort xs)]
    (or
      (= xs ys)
      (= xs (reverse ys)))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not
    (=
      (count a-seq)
      (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [{authors :authors} book]
    (contains? authors author)))

(defn authors [books]
  (set
    (apply concat
      (map :authors books))))

(defn all-author-names [books]
  (set
    (map :name (authors books))))

(defn author->string [{:keys [name birth-year death-year]}]
  (if birth-year
    (str name " (" birth-year " - " death-year ")")
    (str name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [books' (apply str (interpose ". " (map book->string books)))
        n      (count books)]
    (cond
      (= n 0) "No books."
      (= n 1) (str n " book. " books' ".")
      :else    (str n " books. " books' "."))))

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
