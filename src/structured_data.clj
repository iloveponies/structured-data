(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (width rectangle) (height rectangle))))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and (>= px x1)
       (<= px x2)
       (>= py y1)
       (<= py y2)))

(defn contains-rectangle? [outer [inner-p1 inner-p2]]
  (and (contains-point? outer inner-p1)
       (contains-point? outer inner-p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        authors+ (conj authors new-author)]
    (assoc book :authors authors+)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [snd (fn [v] (get v 1))]
    (map snd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (reduce str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [set-authors (set (get book :authors))]
    (assoc book :authors set-authors)))

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (set (map :authors books))))

(defn all-author-names [books]
  (let [author-names
        (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [name (:name author)
        dts (format "(%s - %s)" (:birth-year author) (or (:death-year author) ""))]
    (if (contains? author :birth-year)
      (str name " " dts)
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (format "%s, written by %s" (:title book) (authors->string (:authors book))))

(defn books->string [books]
  (let [cnt (count books)
        book-str (or (and (> cnt 1) "books") "book")
        titles (apply str (interpose ". " (map book->string books)))]
    (if (== cnt 0)
      "No books."
      (str (format "%d %s. %s." cnt book-str titles)))))

(defn books-by-author [author books]
  (filter (fn [b] (contains? (:authors b) author)) books))

(defn author-by-name [name authors]
  (cond (empty? authors) nil
        (= name (:name (first authors))) (first authors)
        :else (author-by-name name (rest authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
