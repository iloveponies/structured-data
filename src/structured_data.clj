(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (or (empty? v) (< (count v) 3))
    nil
    (+ (get v 0) (get v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (or (empty? v) (< (count v) 3))
    nil
    (let [[x y z] v]
      (+ x z))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
      (and (<= x1 px x2) (<= y1 py y2)))))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (get inner 0))
       (contains-point? outer (get inner 1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors
    (let [auctores (:authors book)]
        (conj auctores new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secundus (fn [x] (get x 1))]
    (map secundus collection)))

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
  (assoc book :authors
    (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [author-names
    (fn [boken] (map :name (authors boken)))]
      (set (author-names books))))

(defn author->string [author]
  (str (:name author) (cond
                      (contains? author :death-year)
                        (str " (" (:birth-year author) " - " (:death-year author) ")")
                      (contains? author :birth-year)
                        (str " (" (:birth-year author) " - )")
                      :else
                        "")))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [summary
        (fn [boken]
          (cond (empty? boken) "No books"
                (== 1 (count boken)) "1 book. "
                :else (str (count boken) " books. ")))]
    (let [details
     (fn [bucher] (apply str (interpose ". " (map book->string bucher))))]
    (str (summary books) (details books) "."))))

(defn books-by-author [author books]
  (filter (fn [bb] (has-author? bb author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [nn] (= name (:name nn))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
