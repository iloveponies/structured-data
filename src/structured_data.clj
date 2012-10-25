(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x, y, z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [x (get rectangle 0)
        y (get rectangle 1)
        [a] x
        [b] y]
    (- b a)))

(defn height [rectangle]
  (let [x (get rectangle 0)
        y (get rectangle 1)
        a (get x 1)
        b (get y 1)]
    (- b a)))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [aa (get (get rectangle 0) 0)
        ba (get (get rectangle 1) 0)
        ca (get point 0)
        ab (get (get rectangle 0) 1)
        bb (get (get rectangle 1) 1)
        cb (get point 1)]
    (if (and (<= aa ca ba) (<= ab cb bb))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [aa (get inner 0)
        ba (get inner 1)]
    (if (and (contains-point? outer aa) (contains-point? outer ba))
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
  (let [aiemmat (get book :authors)]
    (assoc book :authors (conj aiemmat new-author))))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

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
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (let [new-book (old-book->new-book book)]
    (if (contains? (:authors new-book) author)
      true
      false)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nam (:name author)]
    (if (contains? author :death-year)
      (str nam " (" (:birth-year author) " - " (:death-year author) ")")
      (if (contains? author :birth-year)
        (str nam " (" (:birth-year author) " - )")
        (str nam)
      ))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [nam (:title book)]
    (str nam ", written by " (authors->string (:authors book)))))

(defn books->string [books]
  (if (empty? books)
    (str "No books.")
    (if (== (count books) 1)
      (str "1 book. " (book->string (first books)) ".")
      (str (count books) " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first(filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))