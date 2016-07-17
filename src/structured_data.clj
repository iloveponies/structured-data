(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [sum (+ (get v 0) (get v 2))]
  (if sum
      sum
      "?")))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v
        sum (+ x z)]
    (if sum
        sum
        "?")))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [z1 z2] point]
    (and (<= x1 z1 x2) (<= y1 z2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[x y] inner]
    (and (contains-point? outer x) (contains-point? outer y))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))

(defn alive? [author]
  (if (contains? author :death-year)
      false
      true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [test (fn [v] (get v 1))]
    (map test collection)))

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
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (author :name)
        synty (author :birth-year)
        kuolema (author :death-year)]
    (if (contains? author :birth-year)
      (str nimi " (" synty " - " kuolema ")")
        (str nimi))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [otsikko (book :title)
        auth (authors->string (book :authors))]
    (str otsikko ", written by " auth)))

(defn books->string [books]
  (let [n (count books)]
    (if (== n 0)
      "No books."
      (if (== n 1)
        (str n " book. " (book->string (first books)) ".")
        (str n " books. " (apply str (interpose ". " (map book->string books)))".")))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (set (filter (fn [x] (= (x :name) name)) authors))))

(defn living-authors [authors]
  (set (filter (fn [x] (alive? x)) authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
