(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    ))

(defn spiff [v]
  (let [a (v 0)
        b (v 2)]
    (+ a b)))

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
  (if (== (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
      (if (and 
           (<= x1 x3 x2)
           (<= y1 y3 y2))
        true
        false))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (if (and
         (contains-point? outer point1)
         (contains-point? outer point2))
      true
      false)))

(defn title-length [book]
  (count (get book :title)))


(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [old-authors (get book :authors)]
    (let [authors (conj old-authors new-author)]
      (assoc book :authors authors))))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))


(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [tokat (fn [v] 
                (let [[x y] v] y))]
    (map tokat collection)))

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
  (if (= (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (let [auth (get book :authors)]
    (assoc book :authors (set auth))))

(defn has-author? [book author]
  (let [book-auth (get book :authors)]
    (contains? book-auth author)))

(defn authors [books]
  (let [auth (map :authors books)]
    (set (apply clojure.set/union auth))))

(defn all-author-names [books]
  (let [just-names (map :name (authors books))]
    (set just-names)))

(defn author->string [author]
  (let [name (:name author)]
    (let [born (:birth-year author)]
      (let [dead (:death-year author)]
        (if (= born nil)
          (str name)
          (str name " (" born " - " dead ")"))))))

(defn authors->string [authors]
  (let [list (map author->string authors)]
    (apply str (interpose ", " list))))

(defn book->string [book]
  (let [title (:title book)]
    (let [authors (authors->string (:authors book))]
      (str title ", written by " authors))))

(defn books->string [books]
  (let [book-c (count books)]
    (if (> 1 book-c)
      (str "No books.")
      (let [mappi (map book->string books)]
        (let [list (apply str (interpose ". " mappi))]
          (if (= 1 book-c)
            (str book-c " book. " list ".")
            (str book-c " books. " list ".")))))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [author] (if (= (:name author) name)
                                true
                                false)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (if (empty? (living-authors authors))
      false
      true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%

