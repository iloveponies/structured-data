(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x z y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (if (> x1 x2) (- x1 x2) (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (if (> y1 y2) (- y1 y2) (- y2 y1))))

(defn square? [rectangle]
  (if (= (height rectangle)(width rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle)(width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [pa pb] point]
    (if (and (<= x1 pa x2) (<= y1 pb y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (if (and (contains-point? outer
                         [x1 y1]) (contains-point? outer [x2 y2])) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [kirj (:authors book)]
    (assoc book :authors (conj kirj new-author))))

(defn alive? [author]
  (let [ika (:death-year author)]
    (if (nil? ika) true false)))

(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [toinen (fn [kokoelma] (second kokoelma))]
    (map toinen collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if (== (count a-seq) (count a-set)) false true)))

(defn old-book->new-book [book]
  (let [kirjailija-nimet (set (:authors book))]
    (assoc book :authors kirjailija-nimet)))

(defn has-author? [book author]
  (let [kirjailijat (:authors book)]
    (if (contains? kirjailijat author) true false)))


(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [names
    (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map names books)))))

(defn author->string [author]
  (let [nimi (:name author)
        synty (:birth-year author)
        kuolo (:death-year author)]
    (if (nil? synty) (str nimi) (apply str [nimi " (" synty " - " kuolo ")"]))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str [(:title book) ", written by " (authors->string (:authors book))]))

(defn books->string [books]
  (let [lukumaara(count books)]
    (cond (= 0 lukumaara) (str "No books.")
          (= 1 lukumaara) (apply str ["1 book. " (apply str (interpose ", " [(apply str (interpose ", " (map book->string books)))])) "."])
          :else (apply str [lukumaara  " books. " (apply str (interpose ", " [(apply str (interpose ", " (map book->string books)))])) "."]))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors(:authors book))) false true))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
