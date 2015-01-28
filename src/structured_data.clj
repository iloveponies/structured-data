(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))
    ;(Math/pow (+ x x) (+ x x))))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
    (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [ptx pty] point]
    (and (and (<= x1 ptx) (>= x2 ptx))
         (and (<= y1 pty) (>= y2 pty)))))

(defn contains-rectangle? [outer inner]
  (let [[ipt1 ipt2] inner]
    (and (contains-point? outer ipt1) 
         (contains-point? outer ipt2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors  (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [func (fn [item] (first (rest item)))]
    (map func collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq)
      (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [orig-count (count a-seq)
        new-count (count (set a-seq))]
    (> orig-count new-count)))

(defn old-book->new-book [book]
  ;(let [auth-vec (:authors book)]
  ;  ()))
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [auth_name (:name author)
        years? (contains? author :birth-year)]
    (if years? 
      (str auth_name " (" (:birth-year author) " - " (:death-year author) ")")
      (str auth_name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str (:title book) ", written by " authors)))

(defn books->string [books]
  (let [num-books (count books)
        books-list-str (apply str(interpose ", " (map book->string books)))]
    (cond (= num-books 1) (str num-books " book. " books-list-str ".")
          (> num-books 1) (str num-books " books. " books-list-str ".")
          :else "No books.")))

(defn books-by-author [author books]
  (let [contains-auth 
        (fn [book] (contains? (authors [book]) author))]
    (filter contains-auth books)))

(defn author-by-name [name authors]
  (let [pred 
        (fn [auth] (= name (:name auth)))]
    (first (filter pred authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (< 0  (count (living-authors (authors [book])))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
