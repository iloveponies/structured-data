(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third )))

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
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [oldies (:authors book)]
    (assoc book :authors (conj oldies new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn apu [x]
  (count x))

(defn element-lengths [collection]
  (map apu collection))

(defn second-elements [collection]
  (let [ota (fn [x] (get x 1))]
  (map ota collection)))

(defn titles [books]
  (let [ota-title (fn [x] (:title x))]
  (map ota-title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
 (if (contains? a-set elem) 
     (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [tekijat (:authors book)]
    (assoc book :authors (set tekijat))))

(defn has-author? [book author]
  (let [tekijat (:authors book)]
    (contains? tekijat author)))

(defn authors [books]
    (let [tekijat 
	(fn [book] (:authors book))]
    (apply clojure.set/union (map tekijat books))))

(defn all-author-names [books]
    (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (:name author)]
  (let [alku (:birth-year author)]
  (let [loppu (:death-year author)]
    (if (nil? alku) 
	(str nimi) 
	(str nimi " (" alku " - " loppu ")")) ))))

(defn authors->string [authors]
  (let [tkj (fn [a] (author->string a))]
    (apply str (interpose ", " (map tkj authors)))))

(defn book->string [book]
  (str (:title book) ", written by " 
       (authors->string (:authors book))))

(defn apulainen [books]
  (let [kirja (fn [a] (book->string a))]
    (apply str (interpose ", " (map kirja books)))))

(defn books->string [books]
  (case (count books)
    0 "No books."
    1 (str "1 book. " (apulainen books) "." )
    (str (count books) " books. " (apulainen books) "." )))

(defn books-by-author [author books]
  (let [kirja (fn [a] (has-author? a author))]
    (filter kirja books)))

(defn author-by-name [name authors]
  (let [t (fn [a] (= (:name a) name ))]
    (first (filter t authors))))

(defn living-authors [authors]
  (let [t (fn [a] (alive? a))]
  (filter t authors)))

(defn has-a-living-author? [book]
  (let [t (fn [a] (alive? a))]
  (not (empty? (filter t (:authors book))))))

(defn books-by-living-authors [books]
  (let [t (fn [b] (has-a-living-author? b))]
  (filter t books)))

; %________%
