(ns structured-data)

(defn do-a-thing [x]
  (let[a (+ x x)]
    (Math/pow a a)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x_b_l y_b_l] [x_t_r y_t_r]] rectangle]
    (- x_t_r x_b_l)))

(defn height [rectangle]
  (let [[[x_b_l y_b_l] [x_t_r y_t_r]] rectangle]
    (- y_t_r y_b_l)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x_b_l y_b_l] [x_t_r y_t_r]] rectangle
        [x y] point]
    (and (<= x_b_l x x_t_r) (<= y_b_l y y_t_r))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [el] (count el)) collection))

(defn second-elements [collection]
  (let [hel (fn [col] (get col 1))]
    (map hel collection)))

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
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [auths (:authors book)]
    (assoc book :authors (set auths))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str
   (:name author)
   (cond
    (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
    (contains? author :birth-year) (str " (" (:birth-year author) " - " ")")
    :else "")
   )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))))

(defn books->string [books]
  (let [books_number (count books)
        prefix (cond
                (= books_number 0) "No books"
                (= books_number 1) "1 book. "
                :else (str books_number " books. "))]
    (str
     prefix
     (apply str (interpose ". " (map book->string books)))
     ".")))

(defn books-by-author [author books]
  (let [this-author? (fn [b] (has-author? b author))]
    (filter this-author? books)))

(defn author-by-name [name authors]
  (let [that-author? (fn [author] (= (:name author) name))]
    (first (filter that-author? authors))))

(defn living-authors [authors]
  (let [dead? (fn [author] (not (contains? author :death-year)))]
    (filter dead? authors)))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (< 0 (count (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
