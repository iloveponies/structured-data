(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (if (> x1 x2) (- x1 x2) (- x2 x1))))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (if (> y1 y2) (- y1 y2) (- y2 y1))))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (if (and (<= y1 yp y2) (<= x1 xp x2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (let [blo (contains-point? outer bl)
        tro (contains-point? outer tr)]
      (and blo tro))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [herpaderpa (fn [x] (get x 1))]
  (map herpaderpa collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [incr (fn [x] (apply <= x))
        decr (fn [x] (apply >= x))]
     (or (incr a-seq) (decr a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (if (nil? (:awards book))
    {:title (:title book)
      :authors (set (:authors book))}
    {:title (:title book)
      :awards (:awards book)
      :authors (set (:authors book))}))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
    (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (:name author)
        synt (:birth-year author)
        deat (:death-year author)]
    (if (contains? author :birth-year) (str nimi " (" synt " - " deat ")") (str nimi))))

(defn authors->string [authors]
  (let [asd (map author->string authors)]
          (apply str (interpose ", " asd))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (== (count books) 0) "No books."
    (str (count books) " book" (if (== (count books) 1) ". " "s. ")
         (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (let [asd (fn [x] (has-author? x author))]
  (filter asd books)))

(defn author-by-name [name authors]
  (let [asd (fn [x] (= name (:name x)))]
    (first (filter asd authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
