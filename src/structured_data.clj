(ns structured-data)

(defn do-a-thing [x]
  (let [xsum (+ x x)]
   (Math/pow xsum xsum)))

(defn spiff [v]
  (+ (or (get v 0) 0) (or (get v 2) 0)))

(defn cutify [v]
  (let [index (count v)]
   (assoc v index "<3")))

(defn spiff-destructuring [v]
    (let [[frst, scnd, thrd] v] (+ frst thrd)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x-bottom-left y-bottom-left][x-top-right y-top-right]] rectangle]
    (- x-top-right x-bottom-left)))

(defn height [rectangle]
  (let [[[x-bottom-left y-bottom-left][x-top-right y-top-right]] rectangle]
    (- y-top-right y-bottom-left)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x-bottom-left-rectangle y-bottom-left-rectangle]
         [x-top-right-rectangle y-top-right-rectangle]] rectangle
        [x-point y-point] point]
      (and
        (<= x-bottom-left-rectangle x-point x-top-right-rectangle)
        (<= y-bottom-left-rectangle y-point y-top-right-rectangle))))

(defn contains-rectangle? [outer inner]
  (let [[left-bottom-inner-point
         right-top-inner-point] inner]
      (and
         (contains-point? outer left-bottom-inner-point)
         (contains-point? outer right-top-inner-point))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (count (get book :authors))))

(defn add-author [book new-author]
  (let [old (get book :authors)]
   (assoc book :authors (conj old  new-author))))

(defn alive? [author]
   (= (get author :death-year) nil))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [vector] (get vector 1))]
    (map get-second collection)))

(defn titles [books]
    (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if
    (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (set(:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (let [authors (:authors book)]
  (contains? authors author)))

(defn authors [books]
  (let [author
         (fn [book] (:authors book))]
    (apply clojure.set/union(map author books))))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [name-part (get author :name)
        death (get author :death-year)
        birth (get author :birth-year)
        date-part (if birth ( str " (" birth " - " death ")"))]
  (str name-part  date-part)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-name (get book :title)
        author (authors->string (:authors book))
        written-by (str ", written by " author)]
    (str book-name written-by)))

(defn books->string [books]
  (let [books-count (count books)
        books-str (cond
                     (= 1 books-count) (str books-count " book. ")
                     (< 1 books-count) (str books-count " books. ")
                     :else "No books.")
        book-descr (if (< 0 books-count)  (str (apply str (interpose ". " (map book->string books))) "."))]
    (apply str books-str book-descr)))

(defn books-by-author [author books]
  (let [by-author (fn [book] (has-author? book author))]
    (filter by-author books)))

(defn author-by-name [name authors]
  (let [by-name (fn [x]  (= name (:name x)))]
    (first (filter by-name authors))))

(defn living-authors [authors]
    (filter alive? authors))

(defn has-a-living-author? [book]
  (let [book-authors (get book :authors )]
      (not (empty? (living-authors book-authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
