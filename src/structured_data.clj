(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

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
  (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [
        [[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [
        [[x3 y3] [x4 y4]] inner]
    (if (and
          (contains-point? outer [x3 y3])
          (contains-point? outer [x4 y4])) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (get book :authors) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-el (fn [x] (get x 1))]
    (map second-el collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (==
        (count a-seq)
        (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(old-book->new-book {:title "The Little Schemer"
                     :authors [friedman, felleisen]})

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))
 
(defn author->string [author]
  (let [name (:name author)
        years (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (if (contains? author :birth-year)
      (str name " " years)
      name)))

(defn authors->string [authors]
  (let [author-strings
        (interpose ", " (map author->string authors))]
    (apply str author-strings)))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [books-number (count books)
        books-string-seq (map book->string books)
        books-string (apply str (interpose ". " books-string-seq))]
    (cond
      (= books-number 1) (str "1 book. " books-string ".")
      (> books-number 1) (str books-number " books. " books-string ".")
      :else "No books. ")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [alives (filter alive? (:authors book))]
    (> (count alives) 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
