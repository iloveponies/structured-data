(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [v-first (get v 0)
        v-third (get v 2)]
    (+ (or v-first 0) (or v-third 0))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v-first v-second v-third] v]
    (+ (or v-first 0) (or v-third 0))))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[[o-x1 o-y1] [o-x2 o-y2]] outer
        [[i-x1 i-y1] [i-x2 i-y2]] inner]
    (and (<= o-x1 i-x1) (<= i-x2 o-x2)
         (<= o-y1 i-y1) (<= i-y2 o-y2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [updated-authors (conj (:authors book) new-author)]
    (assoc book :authors updated-authors)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [is-increasing (apply <= a-seq)
        is-decreasing (apply >= a-seq)]
    (or is-increasing is-decreasing)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (let [a-set-with-elem (conj a-set elem)
        n-elems (count a-set-with-elem)
        n-elems-org (count a-set)
        has-increased (< n-elems-org n-elems)]
    (if has-increased a-set-with-elem (disj a-set elem))))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth-year (:birth-year author)
        death-year (:death-year author)
        years (if (nil? birth-year) "" (str " (" birth-year " - " death-year ")"))]
    (str (:name author) years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n-books (count books)
        n-books-str (cond
                      (= 0 n-books) "No books."
                      (= 1 n-books) "1 book."
                      :else (str n-books " books."))
        add-dot (fn [x] (str x "."))
        book-str-seq (map book->string books)
        books-str (str (apply str (interpose " " (map add-dot book-str-seq))))
        sep (if (= 0 n-books) "" " ")]
    (str n-books-str sep books-str)))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [filter-fn (fn [author] (= name (:name author)))]
    (first (filter filter-fn authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
