(ns structured-data)


(defn do-a-thing [x]
  (let [omg (+ x x)]
    (Math/pow omg omg)))

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

(defn height [rectangle]
  (let [[[a b] [c d]] rectangle]
    (Math/abs (- b d))))

(defn width [rectangle]
  (let [[[a b] [c d]] rectangle]
    (Math/abs (- a c))))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[a b][c d]] rectangle]
    (let [[x y] point]
      (if (or (and (<= a x c) (<= b y d))(and (>= a x c) (>= b y d))) true false))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
      (and (contains-point? outer point1) (contains-point? outer point2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [{lel :title, [{x :birth-year, nonnonnoo :name}] :authors} book]
      (assoc book :authors (conj (:authors book) new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [nobs (fn [x] (first (rest x)))]
    (map nobs collection)))

(defn titles [books]
  (let [ret-title (fn [book] (:title book))]
      (map ret-title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n '*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [{title :title, [{byear :birt-year, nimi :name}] :authors} book]
      (assoc book :authors (set (:authors book)))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors  books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author) (if (contains? author :birth-year) (if (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")") (str " (" (:birth-year author) " - )")) (if (contains? author :death-year) (str " ( - " (:death-year author)) ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (set (map author->string authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (= (count books) 0) "No books."
    (str (count books) (if (= 1 (count books)) " book. " " books. ") (apply str (interpose ". " (set (map book->string books)))) ".")))

(defn books-by-author [author books]
  (let [haz? (fn [book] (has-author? book author))]
      (filter haz? books)))

(defn author-by-name [name authors]
  (let [issame? (fn [author] (= name (:name author)))]
    (if (empty? (filter issame? authors)) nil (into {} (filter issame? authors)))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
