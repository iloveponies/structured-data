(ns structured-data)

(defn do-a-thing [x]
  (let [y x]
    (Math/pow
    (+ x x)
    (+ x x)))
   )

(defn spiff [v]
  (let [a (get v 0)
         b (get v 2)]
        (+ a b))
      )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
    (let [[x b y] v] (+ x y)))

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
     (let [[[x1 y1] [x2 y2]]
     rectangle [x y] point]
     (and (<= x1 x x2)
     (<= y1 y y2)) ))

(defn contains-rectangle? [outer inner]
    (let [ [[x1 y1] [x2 y2]]
     outer rectangle outer
     [[x3 y3] [x4 y4]]
     inner rectangle inner]
     (and (and (<= x1 x3)
     (<= y1 y3)) (and (>= x2 x4)
     (>= y2 y4))) ))

(defn title-length [book]
     (let[ bo (:title book)]
     (count bo)))

(defn author-count [book]
  (let[ bo
  (:authors book)]
  (count bo)))

(defn multiple-authors? [book]
    (let[ bo (:authors book)
     x (count bo)] (> x 1)))

(defn add-author [book new-author]
   (let [nauthor new-author bo
   (book :authors) m (conj bo nauthor)]
  (assoc book :authors m)) )

(defn alive? [author]
  (let [b (contains?
  author :death-year)]
  (not b) ))

(defn element-lengths [collection]
     (let [el (fn [x] (count x))]
     (map el collection) ))

(defn second-elements [collection]
     (let [mg (fn [x] (second (seq x)))]
     (map mg collection)))

(defn titles [books]
   (let [y (fn [book]
    (:title book))]
     (map y books)))

(defn monotonic? [a-seq]
  (let [b (apply <= a-seq)
  c (apply >= a-seq)] (or b c) ) )

(defn stars [n]
   (let [b  (repeat n  "*")]
   (apply str b)) )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem) ))

(defn contains-duplicates? [a-seq]
  (let [b (count a-seq) c (count (set a-seq))]
  (> b c) )  )

(defn old-book->new-book [book]
  (assoc book :authors
  (set (:authors book))) )

(defn has-author? [book author]
   (contains? (:authors book)
    author))

(defn authors [books]
  (let [x (fn [book] (:authors book))]
  (set(apply concat (map x books))) ))


(defn all-author-names [books]
  (let [x (fn [book]
  (map :name (:authors book)))]
  (set(apply concat (map x books))) ))

(defn author->string [author]
   (let [byear (:birth-year author)
    dyear (:death-year author) name
    (:name author) years (str " " "("
     byear " " "-" " " dyear ")")]
    (if byear (str name years) name) ))


(defn authors->string [authors]
   (apply str (interpose ", "
   (map author->string authors))))

(defn book->string [book]
  (let [cc (apply str "written by "
  (interpose ", " (map author->string
  (:authors book)))) ss (:title book)
  bb (apply str (interpose ", " [ss  cc]))
  ] bb ))

(defn books->string [books]
  (let [tt (map :title books)
  cnt (count tt) ll (if (> cnt 1)
  " books. " " book. ") zz (apply
  str (interpose ". "
  (map book->string books)))]
  (if (<= cnt 0) "No books."
  (apply str (apply str cnt ll  zz )"."))
  ))


(defn books-by-author [author books]
  (filter (fn [books] (contains?
  (:authors books) author)) books))


(defn author-by-name [name authors]
  (let [x (filter (fn [authors]
  (= (:name authors) name)) authors)y
  (first (map :name x))] (if (= x ())
  nil (first x) )))

(defn living-authors [authors]
 (filter (fn [authors] (not (contains?
  authors :death-year ))) authors ))

(defn has-a-living-author? [book]
  (let [a (first (:authors book)) b
  (map :death-year a) c (not
  (contains? a :death-year))] c  ))

(defn books-by-living-authors [books]
   (let [x (fn [book] (let [a (first (:authors book))
   c (filter (fn [book] (not (contains? a :death-year))) book)]
    (set (apply concat c))  )) k   (map x books) p (filter
    (fn [k] (not (= k () ))) k)]  (set (apply concat p))))

; %________%