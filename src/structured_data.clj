(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0)(get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [[[x1 y1] [x2 y2]]]
  (if (== (- y2 y1)(- x2 x1))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (if (and (<= x1 x x2)(<= y1 y y2))
    true
    false))

(defn contains-rectangle? [outer [[x1 y1] [x2 y2]]]
  (if (and (contains-point? outer [x1 y1])(contains-point? outer [x2 y2]))
    true
    false))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1)
    true
    false))

(defn add-author [book new-author]
  (if (not (== (author-count book) 0))
    (assoc book :authors (conj (:authors book) new-author))
    (assoc book :authors [new-author])))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [v] (get v 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or(apply <= a-seq)(apply >= a-seq))
    true
    false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count (set a-seq)) (count a-seq))
      false
      true))

(defn old-book->new-book [book]
  (assoc book :authors (set(get book :authors))))

(defn has-author? [book author]
  (if (contains? (get book :authors) author)
    true
    false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [name (:name author) byear (:birth-year author) dyear (:death-year author)]
    (if (nil? byear)
       name
      (str name \space \( byear \space \- \space dyear \)))))

(defn authors->string [authors]
  (let [a (map author->string authors)]
    (apply str (interpose ", " a))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookcount (fn [books]
                    (cond (== 1 (count books)) (str "1 book.")
                          :else (str (count books) " books.")))]
    (cond (== 0 (count books))
          (str "No books.")
    :else (str (bookcount books) \space (apply str(interpose ". " (map book->string books))) \.))))

(defn books-by-author [author books]
  (filter (fn [book] (if (has-author? book author) true false)) books))

(defn author-by-name [name authors]
  (first (set (filter (fn [author] (if (= name (:name author)) true false)) authors))))

(defn living-authors [authors]
  (filter (fn [author] (if (alive? author) true false)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (if (has-a-living-author? book) true false)) books))

; %________%
