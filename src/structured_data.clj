(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]     
     (Math/pow xx xx)))

(defn spiff [v]
  (let [x1 (get v 0)
        x3 (get v 2)]
    (+ x1 x3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x0 _] [x1 _]]]
  (Math/abs (- x0 x1)))

(defn height [[[_ y0] [_ y1]]]
  (Math/abs (- y0 y1)))

(defn square? [r]
  (== (width r) (height r)))

(defn area [r]
  (* (width r) (height r)))

(defn contains-point? [[[x0 y0] [x1 y1]] [x2 y2]]
  (let [in_x (and (<= x0 x2) (<= x2 x1))
        in_y (and (<= y0 y2) (<= y2 y1))]
    (and in_x in_y)))

(defn contains-rectangle? [r [p0 p1]]
  (and (contains-point? r p0) (contains-point? r p1)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

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
  (let [increasing? (apply <= a-seq)
        decreasing? (apply >= a-seq)]
    (or increasing? decreasing?)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [to-set (fn [sq] (set sq))]
    (update-in book [:authors] to-set)))

(defn has-author? [book author]
  (let [authors (get (old-book->new-book book) :authors)]
    (contains? authors author)))

(defn authors [books]
  (let [author-set (fn [book] (set (:authors book)))]
    (apply clojure.set/union (map author-set books))))

(defn all-author-names [books]
  (map :name (authors books)))

(defn author->string [author]
  (let [has-bday      (contains? author :birth-year)
        author-name   (str (:name author))
        author-byear  (str (:birth-year author))
        author-dyear  (str (:death-year author))]
    (if has-bday
      (str author-name " (" author-byear " - " author-dyear ")")
      (str author-name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [b-count   (str (count books)
                       " book"
                       (if (< 1 (count books)) "s" "")
                       ". ")
        b-list    (apply str (interpose ". " (map book->string books)))]
    (str b-count b-list ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name author-seq]
  (first (filter (fn [author] (= (:name author) name)) author-seq)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
