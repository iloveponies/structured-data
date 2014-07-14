(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [ans1 (get v 0)
        ans3 (get v 2)]
    (cond
     (nil? ans1) nil
     (nil? ans3) ans1
     :else (+ ans1 ans3))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[ans1 ans2 ans3] v]
    (cond
     (nil? ans1) nil
     (nil? ans3) ans1
     :else (+ ans1 ans3))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (< 0 (- x1 x2))
      (- x1 x2)
      (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (< 0 (- y1 y2))
      (- y1 y2)
      (- y2 y1))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
  (and (or (<= x1 x x2) (>= x1 x x2))
       (or (<= y1 y y2) (>= y1 y y2)))))

(defn contains-rectangle? [outer inner]
  (let [[fst snd] inner]
    (and (contains-point? outer fst)
         (contains-point? outer snd))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [orig-auth (:authors book)]
    (assoc book :authors (conj orig-auth new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

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
    (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [a author]
    (if (nil? (:birth-year a))
      (str (:name a))
      (str (:name a) " ("
           (:birth-year a) " - "
           (:death-year a) ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (< 1 (count books)) (str (count books) " books. "
                             (apply str (interpose ". "
                                                   (map book->string books))) ".")
    (== 1 (count books)) (str "1 book. " (book->string (first books)) ".")
    :else "No books."))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [list1 (filter (fn [x] (= (:name x) name)) authors)]
    (if (empty? list1)
      nil
      (first list1))))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
