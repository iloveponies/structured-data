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
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (- xtr xbl)))

(defn height [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (- ytr ybl)))

(defn square? [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (== (- ytr ybl) (- xtr xbl))))

(defn area [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (* (- ytr ybl) (- xtr xbl))))

(defn contains-point? [rectangle point]
    (let [[[xbl ybl] [xtr ytr]] rectangle
          [xp yp] point]
    (and (<= xbl xp xtr) (<= ybl yp ytr))))

(defn contains-rectangle? [outer inner]
  (let [[pointbl pointtr] inner]
    (and (contains-point? outer pointbl) (contains-point? outer pointtr))))

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
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (map (fn [x] (first (rest x))) collection))

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
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [n (:name author)
       by (:birth-year author)
       dy (:death-year author)]
    (if (contains? author :birth-year)
  (str n " (" by " - " dy ")")
  (str n))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book) )))

(defn books->string [books]
  (let [cnt (count books)
        bookstring (apply str (interpose ". " (map book->string books)))]
    (cond
      (== cnt 0) (str "No books.")
      (== cnt 1) (str cnt " book. " bookstring ".")
      :else      (str cnt " books. " bookstring "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
