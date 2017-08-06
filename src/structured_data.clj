(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
      (and (<= x1 px x2) (<= y1 py y2)))))

(defn contains-rectangle? [outer inner]
  (let [[i1 i2] inner]
    (and (contains-point? outer i1) (contains-point? outer i2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
 (let [newauthors (conj (:authors book) new-author)]
   (assoc book :authors newauthors)))

(defn alive? [author]
  (not (boolean (:death-year author))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [toka (fn [collection] (get collection 1))]
    (map toka collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (str (:name author))]
    (if (boolean (:birth-year author)) (let [synt (str (:birth-year author))]
      (if (boolean (:death-year author)) (let [kuol (str (:death-year author))]
                                          (str nimi " (" synt " - " kuol ")"))
                   (str nimi " (" synt " - )")))
      nimi)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by "
  (authors->string (set (:authors book)))))

(defn books->string [books]
  (let [maara (count books)]
    (if (== maara 0) "No books."
      (if (== maara 1)
      (let [alku (str maara " book. ")]
        (let [kirjat (apply str (interpose ". " (map book->string books)))]
          (str alku kirjat ".")))
        (let [alku (str maara " books. ")]
        (let [kirjat (apply str (interpose ". " (map book->string books)))]
          (str alku kirjat ".")))))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [turhalista (filter (fn [author] (= name (:name author))) authors)]
  (if (== 0 (count turhalista)) nil
    (first turhalista))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))




; %________%
